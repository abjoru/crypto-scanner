package com.bjoru.cscanner.eth

import cats.syntax.traverse.given
import cats.effect.IO

import io.circe.*
import io.circe.syntax.{*, given}

import org.http4s.dsl.io.*
import org.http4s.client.*
import org.http4s.client.dsl.io.*
import org.http4s.circe.*
import org.http4s.circe.CirceEntityDecoder.given
import org.http4s.implicits.uri

import com.bjoru.cscanner.{*, given}
import com.bjoru.cscanner.types.*
import com.bjoru.cscanner.utils.*
import com.bjoru.cscanner.config.*

import java.nio.file.Path

object EthplorerProvider:

  import Balance.*
  import Quantity.decodeQuantity

  given Decoder[TokenBalance] = Decoder.instance { c =>
    for name <- c.downField("tokenInfo").downField("name").as[String]
        symb <- c.downField("tokenInfo").downField("symbol").as[Symbol]
        dec  <- c.downField("tokenInfo").downField("decimals").as[Int]
        addr <- c.downField("tokenInfo").downField("address").as[Address]
        bala <- c.downField("rawBalance").as[String]
        rate  = c.downField("tokenInfo").downField("price").downField("rate").success.flatMap(checkPrice)
        res  <- decodeQuantity(Token(symb, name, dec, Some(addr), rate), bala).circeResult(c)
    yield TokenBalance(Token(symb, name, dec, Some(addr), rate), res)
  }

  private val Uri = uri"https://api.ethplorer.io/getAddressInfo"

  def tokenBalances(wallet: Wallet)(client: Client[IO]): IO[Seq[TokenBalance]] =
    client.expect[Json](Uri / wallet.address.stringValue +? ("apiKey" -> "freekey")).flatMap { json => 
      for wei  <- IO.fromEither(json.hcursor.downField("ETH").downField("rawBalance").as[String])
          tail <- IO.fromEither(json.hcursor.downField("tokens").as[Seq[TokenBalance]])
      yield TokenBalance(Token.Eth, fromWei(wei, Unit.Ether)) +: tail
    }

  private def checkPrice(c: HCursor) = c.as[Double].toOption
