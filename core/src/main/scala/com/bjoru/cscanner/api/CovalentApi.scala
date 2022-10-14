package com.bjoru.cscanner.api

import cats.effect.IO

import io.circe.*
import io.circe.syntax.{*, given}

import org.http4s.Uri
import org.http4s.dsl.io.*
import org.http4s.client.*
import org.http4s.client.dsl.io.*
import org.http4s.circe.*
import org.http4s.circe.CirceEntityDecoder.given

import com.bjoru.cscanner.{*, given}
import com.bjoru.cscanner.types.*
import com.bjoru.cscanner.utils.*
import com.bjoru.cscanner.config.findEndpoint
import Quantity.decodeQuantity

import scala.util.{Try, Success, Failure}

import java.nio.file.Path

// Ref: https://github.com/darrylyeo/blockhead

enum Dex(val id: String, chains: Chain*):
  case Uniswap_v2     extends Dex("uniswap_v2", Chain.Ethereum)
  case SushiSwap      extends Dex("sushiswap", Chain.Ethereum, Chain.Polygon, Chain.Fantom, Chain.Avalanche, Chain.Binance)
  case ApeSwap_v2     extends Dex("apeswap_v2", Chain.Binance)
  case QuickSwap      extends Dex("quickswap", Chain.Polygon)
  case SpiritSwap     extends Dex("spiritswap", Chain.Fantom)
  case SpookySwap     extends Dex("spookyswap", Chain.Fantom)
  case Pangolin       extends Dex("pangolin", Chain.Avalanche)
  case TraderJoe      extends Dex("traderjoe", Chain.Avalanche)
  case DefiKingdoms   extends Dex("defi-kingdoms", Chain.Harmony, Chain.Avalanche)
  case Moonlift       extends Dex("moonlift", Chain.Binance)
  case PancakeSwap_v2 extends Dex("pancakeswap_v2", Chain.Binance)
  case EmpireDex      extends Dex("empiredex", Chain.Binance)
  //case ArthSwap       extends Dex("arthswap")
  //case YokaiSwap      extends Dex("yokaiswap")
  //case ClaimSwap      extends Dex("claimswap")
  //case Diffusion      extends Dex("diffusion")
  //case Cronus         extends Dex("cronus")
  //case EvmoSwap       extends Dex("evmoswap")
  //case Mimo           extends Dex("mimo")
  //case Standard       extends Dex("standard")
  //case Katana         extends Dex("katana")
  //case Vvs            extends Dex("vvs")
  //case Mmf            extends Dex("mmf")
  //case StellaSwap     extends Dex("stellaswap")
  //case BeamSwap       extends Dex("beamswap")
  //case Trisolaris     extends Dex("trisolaris")
  //case WannaSwap      extends Dex("wannaswap")

class CovalentApi(cfgDir: Path, chain: Chain):

  import Balance.*

  given Decoder[TokenBalance] = Decoder.instance { c =>
    for name <- c.downField("contract_name").as[String]
        symb <- c.downField("contract_ticker_symbol").as[Symbol]
        dec  <- c.downField("contract_decimals").as[Int]
        addr <- c.downField("contract_address").as[Address]
        bal  <- c.downField("balance").as[BigDecimal]
        rate <- c.downField("quote_rate").as[Double]
        conv <- decodeQuantity(Token(symb, name, dec, None), bal).circeResult(c)
    yield TokenBalance(Token(symb, name, dec, Some(addr), Some(rate)), conv)
  }

  val chainId = chain match
    case Chain.Polygon   => Right("137")
    case Chain.Avalanche => Right("43114")
    case Chain.Fantom    => Right("250")
    case Chain.Harmony   => Right("1666600000")
    case Chain.Binance   => Right("56")
    case Chain.Solana    => Right("1399811149")
    case _               => Left(new Exception("Unsupported CovalentHQ chain!"))

  def balanceUri(addr: Address) = 
    for c <- IO.fromEither(chainId)
        p <- findEndpoint(cfgDir </> "endpoints.yaml", Provider.CovalentHQ, chain)
        u <- IO.fromEither(Uri.fromString(s"${p.uri.toString}/$c/address/${addr.stringValue}/balances_v2/?key=${p.apiKey}"))
    yield u

  def dexBalanceUri(addr: Address, dex: String) =
    for c <- IO.fromEither(chainId)
        p <- findEndpoint(cfgDir </> "endpoints.yaml", Provider.CovalentHQ, chain)
        u <- IO.fromEither(Uri.fromString(s"${p.uri.toString}/$c/xy=k/$dex/address/${addr.stringValue}/balances/?key=${p.apiKey}"))
    yield u

  def tokenBalance(wallet: Wallet)(client: Client[IO]): IO[Seq[TokenBalance]] = 
    for u <- balanceUri(wallet.address)
        j <- client.expect(u)(jsonOf[IO, Json])
        r <- IO.fromEither(j.hcursor.downField("data").downField("items").as[Seq[TokenBalance]])
    yield r

  //https://api.covalenthq.com/v1/:chain_id/xy=k/:dexname/address/:address/balances/?&key=
  def dexBalance(wallet: Wallet, dex: Dex)(client: Client[IO]): IO[Json] =
    for u <- dexBalanceUri(wallet.address, dex.id)
        j <- client.expect(u)(jsonOf[IO, Json])
    yield j
