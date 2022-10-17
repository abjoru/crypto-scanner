package com.bjoru.cryptos.api

import cats.implicits.given
import cats.effect.IO

import io.circe.*

import org.http4s.client.*
import org.http4s.implicits.uri
import org.http4s.circe.*
import org.http4s.circe.CirceEntityDecoder.given

import com.bjoru.cryptos.types.*

import scala.util.{Try, Success, Failure}

class BlockCypherApi(endpoint: Endpoint) extends CryptoApi:

  val supportedChains = Seq(Chain.Bitcoin, Chain.Dogecoin)

  //private val baseUri = uri"https://api.blockcypher.com/v1"

  protected def doSyncWallets(wallets: Seq[Wallet], client: Client[IO]): IO[Seq[Wallet]] =
    wallets.traverse(tokenBalance(_, client))

  def tokenBalance(wallet: Wallet, client: Client[IO]): IO[Wallet] =
    for a <- IO.pure(endpoint.uri / wallet.chain.symbol.lower / "main" / "addrs" / wallet.address.str)
        b <- client.expect(a)(jsonOf[IO, Json])
        c <- IO.fromEither(b.hcursor.downField("balance").as[BigInt])
        d <- IO.fromOption(Token.forChain(wallet.chain))(new Exception(s"Unsupported chain!"))
    yield wallet.withTokens(Seq(d.withRawBalance(c)))
