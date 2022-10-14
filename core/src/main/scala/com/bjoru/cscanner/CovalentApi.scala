package com.bjoru.cscanner

import cats.effect.IO

import io.circe.*
import io.circe.syntax.{*, given}

import org.http4s.Uri
import org.http4s.dsl.io.*
import org.http4s.client.*
import org.http4s.client.dsl.io.*
import org.http4s.circe.*
import org.http4s.circe.CirceEntityDecoder.given

import com.bjoru.cscanner.types.*
import com.bjoru.cscanner.utils.*
import Quantity.decodeQuantity

class CovalentApi(chain: Chain):

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
    case Chain.Polygon   => 137
    case Chain.Avalanche => 43114
    case Chain.Fantom    => 250
    case Chain.Harmony   => 1666600000
    case Chain.Binance   => 56
    case Chain.Solana    => 1399811149
    case _               => throw new Exception("Unsupported chain")


  // FIXME externalize apikey
  val ApiKey = "ckey_fe6f984e42eb4a11a265e659362"

  def balanceUri(addr: Address): Uri = Uri.unsafeFromString(
    s"https://api.covalenthq.com/v1/$chainId/address/${addr.stringValue}/balances_v2/?key=$ApiKey"
  )

  def tokenBalance(wallet: Wallet)(client: Client[IO]): IO[Seq[TokenBalance]] = 
    client.expect(balanceUri(wallet.address))(jsonOf[IO, Json]).flatMap { json =>
      IO.fromEither(json.hcursor.downField("data").downField("items").as[Seq[TokenBalance]])
    }
