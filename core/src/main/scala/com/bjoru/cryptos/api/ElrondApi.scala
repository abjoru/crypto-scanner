package com.bjoru.cryptos.api

import cats.implicits.given
import cats.effect.IO

import io.circe.*

import org.http4s.client.*
import org.http4s.implicits.uri
import org.http4s.circe.*
import org.http4s.circe.CirceEntityDecoder.given

import com.bjoru.cryptos.types.*
import com.bjoru.cryptos.syntax.*

import scala.util.{Try, Success, Failure}

class ElrondApi(endpoint: Endpoint) extends CryptoApi:

  given Decoder[Token] = Decoder.instance { c =>
    for name <- c.downField("name").as[String]
        symb <- c.downField("ticker").as[Symbol]
        dec  <- c.downField("decimals").as[Int]
        usd  <- c.downField("valueUsd").as[Double]
        bal  <- c.downField("balance").as[String]
        tok   = Token(name, symb, dec, None, Some(usd), None)
        res  <- tok.withRawBalance(bal).circeResult(c)
    yield res
  }

  val supportedChains = Seq(Chain.Elrond)

  protected def doSyncWallets(wallets: Seq[Wallet], client: Client[IO]): IO[Seq[Wallet]] =
    wallets.traverse(tokenBalance(_, client))

  def tokenBalance(wallet: Wallet, client: Client[IO]): IO[Wallet] =
    for u <- IO.pure(endpoint.uri / "accounts" / wallet.address.str / "tokens")
        t <- client.expect[Seq[Token]](u)
    yield wallet.withTokens(t)
