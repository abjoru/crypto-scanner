package com.bjoru.cryptosis.providers

import cats.effect.IO
import cats.syntax.traverse.given

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

  protected def doSync(wallets: Seq[Wallet])(using Client[IO]): IO[Seq[Wallet]] =
    for _ <- putStrLn("elrond: starting wallet sync..")
        a <- wallets.traverse(balances)
        b <- a.traverse(egld)
        _ <- putStrLn("elrond: finished wallet sync.")
    yield b

  def egld(wallet: Wallet)(using cl: Client[IO]): IO[Wallet] =
    for a <- cl.expect(ep.uri / "accounts" / wallet.address.str)(jsonOf[IO, Json])
        b <- Env.bluechipToken(Chain.Elrond)
        c <- IO.fromEither(a.hcursor.downField("balance").as[String])
        d  = Balance.fromRaw(c, b.decimals)
        e  = b.withBalance(b.decimals, d)
        t <- Env.resolveToken(e)
    yield wallet.addBalance(t)

  def balances(wallet: Wallet)(using cl: Client[IO]): IO[Wallet] =
    for a <- cl.expect[Seq[Token]](ep.uri / "accounts" / wallet.address.str / "tokens")
        r <- Env.resolveTokens(filter(Chain.Elrond, a))
    yield wallet.addBalances(r)

  private def filter(c: Chain, tx: Seq[Token]) =
    filters.find(_.chain == c).map(_.filterTokens(tx)).getOrElse(tx)
