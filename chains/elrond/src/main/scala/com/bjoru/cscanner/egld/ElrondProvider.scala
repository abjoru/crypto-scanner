package com.bjoru.cscanner.egld

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

object ElrondProvider:

  import Quantity.decodeTokenQuantity as decodeTQ

  given Decoder[TokenBalance] = Decoder.instance { c =>
    for name <- c.downField("name").as[String]
        symb <- c.downField("ticker").as[Symbol]
        dec  <- c.downField("decimals").as[Int]
        bal  <- c.downField("balance").as[String]
        usd  <- c.downField("valueUsd").as[Double]
        res  <- decodeTQ(Token(symb, name, dec, None, Some(usd)), bal).circeResult(c)
    yield res
  }

  private val Uri = uri"https://api.elrond.com"

  def getTokens(wallet: Wallet)(client: Client[IO]): IO[Seq[TokenBalance]] =
    client.expect[Seq[TokenBalance]](Uri / "account" / wallet.address.stringValue / "tokens")
