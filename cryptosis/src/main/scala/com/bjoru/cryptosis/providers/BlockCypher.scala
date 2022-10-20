package com.bjoru.cryptosis.providers

import cats.effect.IO
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

  protected def doSync(wallets: Seq[Wallet], client: Client[IO], env: Env): IO[(Env, Seq[Wallet])] =
    wallets.foldLeftM(env -> Seq.empty[Wallet]) {
      case ((env, acc), w) => walletBalance(w, client, env).map(kv => kv._1 -> (acc :+ kv._2))
    }

  private def walletBalance(wallet: Wallet, client: Client[IO], env: Env): IO[(Env, Wallet)] =
    for a <- IO.pure(ep.uri / wallet.chain.symbol.lower / "main" / "addrs" / wallet.address.str)
        b <- client.expect(a)(jsonOf[IO, Json])
        c <- IO.fromEither(b.hcursor.downField("balance").as[BigInt])
        d <- processToken(env, wallet.chain, c)
    yield d._1 -> wallet.addBalance(d._2)

  private def processToken(env: Env, chain: Chain, balance: BigInt): IO[(Env, Token)] = 
    for t <- IO.fromTry(env.bluechipToken(chain))
        u  = t.withBalance(t.decimals, Balance(balance))
        e  = env.updateToken(t)
    yield e -> t
