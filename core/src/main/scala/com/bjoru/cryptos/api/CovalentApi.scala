package com.bjoru.cryptos.api

import cats.syntax.traverse.given
import cats.effect.IO

import io.circe.*
import io.circe.syntax.{*, given}

import org.http4s.Uri
import org.http4s.dsl.io.*
import org.http4s.client.*
import org.http4s.client.dsl.io.*
import org.http4s.circe.*
import org.http4s.circe.CirceEntityDecoder.given

import scala.util.{Try, Success, Failure}

import com.bjoru.cryptos.types.*
import com.bjoru.cryptos.utils.{*, given}
import com.bjoru.cryptos.config.*

import java.nio.file.Path

class CovalentApi(endpoint: Endpoint, filters: Seq[TokenFilter]) extends CryptoApi:

  given Decoder[Token] = Decoder.instance { c =>
    for name <- c.downField("contract_name").as[String]
        sym  <- c.downField("contract_ticker_symbol").as[Symbol]
        dec  <- c.downField("contract_decimals").as[Int]
        addr <- c.downField("contract_address").as[Address]
        bal  <- c.downField("balance").as[BigInt]
        rate <- c.downField("quote_rate").as[BigDecimal]
    yield Token(name, sym, dec, Some(addr), Some(Usd(rate)), None).withRawBalance(bal)
  }

  val supportedChains = Seq(
    Chain.Ethereum,
    Chain.Binance,
    Chain.Avalanche,
    Chain.Fantom,
    Chain.Polygon,
    Chain.Harmony,
    Chain.Solana
  )

  private def chainId(chain: Chain) = chain match
    case Chain.Ethereum  => Right("1")
    case Chain.Polygon   => Right("137")
    case Chain.Avalanche => Right("43114")
    case Chain.Fantom    => Right("250")
    case Chain.Harmony   => Right("1666600000")
    case Chain.Binance   => Right("56")
    case Chain.Solana    => Right("1399811149")
    case other           => Left(new Exception(s"Unsupported CovalentHQ chain: $other!"))

  private def balanceUri(wallet: Wallet) =
    for c <- IO.fromEither(chainId(wallet.chain))
        u <- IO.fromEither(Uri.fromString(s"${endpoint.uri.toString}/$c/address/${wallet.address.str}/balances_v2/?key=${endpoint.apiKey}"))
    yield u

  def tokenBalance(wallet: Wallet, client: Client[IO]): IO[Wallet] =
    for u <- balanceUri(wallet)
        j <- client.expect(u)(jsonOf[IO, Json])
        f  = filters.find(_.chain == wallet.chain)
        t  = j.hcursor.downField("data").downField("items").values.map(processResult).getOrElse(Iterable.empty)
        r  = f.map(_.filterTokens(t.toSeq)).getOrElse(t.toSeq)
    yield wallet.withTokens(r)

  def multichainBalance(wallet: Wallet, client: Client[IO]): IO[Seq[Wallet]] =
    val chains = supportedChains.filterNot(_ == Chain.Solana)
    val result =  chains.traverse(c => tokenBalance(wallet.withChain(c), client))
    result //.map(_.reduce((a, b) => a.withTokens(b.tokens)))

  def doSyncWallets(wallets: Seq[Wallet], client: Client[IO]): IO[Seq[Wallet]] =
    val (mw, sw) = wallets.partition(_.isMultichain)

    for 
      mc <- mw.traverse(multichainBalance(_, client))
      sc <- sw.traverse(tokenBalance(_, client))
    yield mc.flatten ++ sc

  private def processResult(items: Iterable[Json]) = 
    items.map(_.as[Token].toOption).flatten

