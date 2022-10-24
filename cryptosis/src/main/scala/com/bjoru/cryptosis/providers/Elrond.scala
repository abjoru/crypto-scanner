package com.bjoru.cryptosis.providers

import cats.effect.IO
import cats.syntax.foldable.given

import io.circe.*
import io.circe.syntax.{*, given}

import org.http4s.dsl.io.*
import org.http4s.client.*
import org.http4s.client.dsl.io.*
import org.http4s.circe.*
import org.http4s.circe.CirceEntityDecoder.given
import org.http4s.implicits.uri

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*

class Elrond(ep: Endpoint, filters: Seq[TokenFilter]) extends ProviderApi:

  import Token.PricedAndBalancedToken as PToken

  given Decoder[Token] = Decoder.instance { hc =>
    for a <- hc.downField("name").as[String]
        b <- hc.downField("ticker").as[Symbol]
        c <- hc.downField("decimals").as[Int]
        d <- hc.downField("balance").as[String]
        e <- hc.downField("valueUsd").as[Price]
        p  = Balance.fromRaw(d, c)
    yield PToken(a.toLowerCase, a, b, Chain.Elrond, c, None, p, e)
  }

  val supportedChains = Seq(Chain.Elrond)

  protected def doSync(wallets: Seq[Wallet], env: Env)(using Client[IO]): IO[(Env, Seq[Wallet])] =
    for a <- foreachWallet(env, wallets)(balances)
        b <- foreachWallet(a._1, a._2)(egld)
    yield b

  def egld(env: Env, wallet: Wallet)(using cl: Client[IO]): IO[(Env, Wallet)] =
    for a <- cl.expect(ep.uri / "accounts" / wallet.address.str)(jsonOf[IO, Json])
        b <- IO.fromTry(env.bluechipToken(Chain.Elrond))
        c <- IO.fromEither(a.hcursor.downField("balance").as[String])
        d  = Balance.fromRaw(c, b.decimals)
        e  = b.withBalance(b.decimals, d)
    yield env.updateToken(e) -> wallet.addBalance(e)

  def balances(env: Env, wallet: Wallet)(using cl: Client[IO]): IO[(Env, Wallet)] =
    for a <- cl.expect[Seq[Token]](ep.uri / "accounts" / wallet.address.str / "tokens")
        r  = env.resolveAndUpdateAll(filter(Chain.Elrond, a))
    yield r._1 -> wallet.addBalances(r._2)

  private def filter(c: Chain, tx: Seq[Token]) =
    filters.find(_.chain == c).map(_.filterTokens(tx)).getOrElse(tx)
