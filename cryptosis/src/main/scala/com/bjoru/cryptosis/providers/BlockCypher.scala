package com.bjoru.cryptosis.providers

import cats.effect.IO
import cats.syntax.show.given
import cats.syntax.foldable.given
import cats.syntax.traverse.given

import io.circe.*

import org.http4s.client.*
import org.http4s.implicits.uri
import org.http4s.circe.*
import org.http4s.circe.CirceEntityDecoder.given

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*

class BlockCypher(ep: Endpoint) extends ProviderApi:

  val supportedChains = Seq(Chain.Bitcoin, Chain.Dogecoin)

  protected def doSync(wallets: Seq[Wallet])(using Client[IO]): IO[Seq[Wallet]] =
    for _ <- putStrLn("blockcypher: starting wallet sync..")
        r <- wallets.traverse(walletBalance)
        _ <- putStrLn("blockcypher: finished wallet sync.")
    yield r

  private def walletBalance(wallet: Wallet)(using client: Client[IO]): IO[Wallet] =
    for a <- IO.pure(ep.uri / wallet.chain.symbol.lower / "main" / "addrs" / wallet.address.str)
        b <- client.expect(a)(jsonOf[IO, Json])
        c <- IO.fromEither(b.hcursor.downField("balance").as[BigInt])
        d <- processToken(wallet.chain, c)
    yield wallet.addBalance(d)

  private def processToken(chain: Chain, balance: BigInt)(using Client[IO]): IO[Token] = 
    for t <- Env.bluechipToken(chain)
        u  = t.withBalance(t.decimals, Balance.fromRaw(balance, t.decimals))
    yield u
