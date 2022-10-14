package com.bjoru.cscanner.sol

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

object Solscan:

  import Quantity.decodeTokenQuantity as decodeTQ

  given Decoder[TokenBalance] = Decoder.instance { c =>
    for name <- c.downField("tokenName").as[String]
        symb  = c.downField("tokenSymbol").success.flatMap(_.as[Symbol].toOption)
        dec  <- c.downField("tokenAmount").downField("decimals").as[Int]
        addr <- c.downField("tokenAddress").as[Address]
        bala <- c.downField("tokenAmount").downField("amount").as[String]
        res  <- decodeTQ(Token(symb.getOrElse(Symbol("")), name, dec, Some(addr)), bala).circeResult(c)
    yield res
  }

  private val Uri      = uri"https://public-api.solscan.io/account/tokens"
  private val StakeUri = uri"https://public-api.solscan.io/account/stakeAccount"

  def tokenBalances(wallet: Wallet)(client: Client[IO]): IO[Seq[TokenBalance]] =
    client.expect[Seq[TokenBalance]](Uri +? ("account" -> wallet.address.stringValue))
          .map(_.filter(_._1.symbol.nonEmpty)) // drop scam coins

  def stakingBalance(wallet: Wallet)(client: Client[IO]): IO[Option[TokenBalance]] =
    client.expect(StakeUri +? ("account" -> wallet.address.stringValue))(jsonOf[IO, Json]).map { json =>
      json.hcursor.downField(wallet.address.stringValue).success.flatMap { c =>
        c.downField("amount").as[String].flatMap(decodeTQ(Token.Sol, _).circeResult(c)).toOption
      }
    }
