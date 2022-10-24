package com.bjoru.cryptosis.providers

import cats.effect.IO
import cats.syntax.show.given
import cats.syntax.foldable.given

import io.circe.*

import org.http4s.client.*
import org.http4s.implicits.uri
import org.http4s.circe.*
import org.http4s.circe.CirceEntityDecoder.given

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*

class BlockCypher(ep: Endpoint) extends ProviderApi:

  val supportedChains = Seq(Chain.Bitcoin, Chain.Dogecoin)

  protected def doSync(wallets: Seq[Wallet], env: Env)(using Client[IO]): IO[(Env, Seq[Wallet])] =
    foreachWallet(env, wallets)(walletBalance)

  private def walletBalance(env: Env, wallet: Wallet)(using client: Client[IO]): IO[(Env, Wallet)] =
    for a <- IO.pure(ep.uri / wallet.chain.symbol.lower / "main" / "addrs" / wallet.address.str)
        b <- client.expect(a)(jsonOf[IO, Json])
        c <- IO.fromEither(b.hcursor.downField("balance").as[BigInt])
        d <- processToken(env, wallet.chain, c)
    yield env -> wallet.addBalance(d)

  private def processToken(env: Env, chain: Chain, balance: BigInt): IO[Token] = 
    for t <- IO.fromTry(env.bluechipToken(chain))
        u  = t.withBalance(t.decimals, Balance.fromRaw(balance, t.decimals))
    yield u
