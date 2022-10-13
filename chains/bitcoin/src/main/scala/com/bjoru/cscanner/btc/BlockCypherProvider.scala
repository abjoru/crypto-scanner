package com.bjoru.cscanner.btc

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

object BlockCypherProvider:

  import Quantity.decodeTokenQuantity as decodeTQ

  given Decoder[TokenBalance] = Decoder.instance { c =>
    c.downField("balance").as[Long].flatMap { bal =>
      decodeTQ(Token.Btc, bal.toString).circeResult(c)
    }
  }

  private val Uri = uri"https://api.blockcypher.com/v1/btc/main/addrs"

  def tokenBalance(wallet: Wallet)(client: Client[IO]): IO[TokenBalance] = 
    client.expect[TokenBalance](Uri / wallet.address.stringValue)
