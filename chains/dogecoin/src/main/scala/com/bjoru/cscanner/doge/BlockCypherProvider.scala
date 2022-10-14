package com.bjoru.cscanner.doge

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

  import Balance.TokenBalance
  import Quantity.decodeQuantity

  given Decoder[TokenBalance] = Decoder.instance { c =>
    c.downField("balance").as[Long].flatMap { bal =>
      decodeQuantity(Token.Doge, bal.toString)
        .circeResult(c)
        .map(TokenBalance(Token.Doge, _))
    }
  }

  private val Uri = uri"https://api.blockcypher.com/v1/doge/main/addrs"

  def tokenBalance(wallet: Wallet)(client: Client[IO]): IO[TokenBalance] = 
    client.expect[TokenBalance](Uri / wallet.address.stringValue)
  
