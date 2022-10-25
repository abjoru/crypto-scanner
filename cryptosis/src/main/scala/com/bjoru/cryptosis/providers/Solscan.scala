package com.bjoru.cryptosis.providers

import cats.effect.IO
import cats.syntax.traverse.given
import cats.syntax.foldable.given

import io.circe.*
import io.circe.syntax.{*, given}

import org.http4s.Uri
import org.http4s.dsl.io.*
import org.http4s.client.*
import org.http4s.client.dsl.io.*
import org.http4s.circe.*
import org.http4s.circe.CirceEntityDecoder.given

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*
import com.bjoru.cryptosis.syntax.circe.*

class Solscan(ep: Endpoint, filters: Seq[TokenFilter]) extends ProviderApi:

  given Decoder[Token] = Decoder.instance { hc =>
    for a <- hc.downField("tokenAddress").as[Address]
        b <- hc.downField("tokenAmount").downField("amount").as[String]
        c <- hc.downField("tokenAmount").downField("decimals").as[Int]
        d <- hc.downField("tokenName").as[String]
        e <- hc.downField("tokenSymbol").as[Symbol]
        f  = Balance.fromRaw(b, c)
    yield Token(d, d, e, Chain.Solana, c, Some(a)).withBalance(c, f)
  }

  val LAMPORTS_PR_SOL = BigDecimal(1000000000.0)

  val supportedChains = Seq(Chain.Solana)

  protected def doSync(wallets: Seq[Wallet])(using Client[IO]): IO[Seq[Wallet]] =
    for a <- wallets.traverse(solBalance)
        b <- a.traverse(balances)
        c <- b.traverse(balances)
    yield c

  private def solBalance(wallet: Wallet)(using client: Client[IO]): IO[Wallet] =
    for a <- IO.pure(ep.uri / "account" / wallet.address.str)
        b <- client.expect(a)(jsonOf[IO, Json])
        c <- (b <\> "lamports").asIO[BigDecimal]
        d <- Env.bluechipToken(Chain.Solana)
        e  = d.withBalance(d.decimals, Balance.fromBigDecimal(c / LAMPORTS_PR_SOL))
    yield wallet.addBalance(e)

  private def balances(wallet: Wallet)(using client: Client[IO]): IO[Wallet] = 
    for a <- IO.pure(ep.uri / "account" / "tokens" +? ("account" -> wallet.address.str))
        b <- client.expect(a)(jsonOf[IO, Seq[Json]])
        c <- b.traverse(j => (j <\> "tokenSymbol").asIO[Option[Symbol]].map(j -> _))
        d <- IO.fromEither(c.filter(_._2.isDefined).traverse(_._1.as[Token]))
        e <- Env.resolveTokens(d)
    yield wallet.addBalances(e)

  private def staking(wallet: Wallet)(using client: Client[IO]): IO[Wallet] =
    for a <- IO.pure(ep.uri / "account" / "stakeAccounts" +? ("account" -> wallet.address.str))
        b <- client.expect(a)(jsonOf[IO, Json])
        c  = b.hcursor.keys.map(_.head)
        d <- parseStake(c, b)
    yield d.map(wallet.addBalance(_)).getOrElse(wallet)

  private def parseStake(key: Option[String], json: Json)(using Client[IO]): IO[Option[Defi.Stake]] =
    key match
      case Some(k) =>
        for a <- (json <\> k).asIO[Json]
            b <- (a <\> "amount").asIO[String]
            c <- Env.bluechipToken(Chain.Solana)
            d  = c.withBalance(c.decimals, Balance.fromBigDecimal(BigDecimal(b) / LAMPORTS_PR_SOL))
        yield Some(Defi.Stake("solscan-sol-staking", "Sol Staking", Chain.Solana, Seq(d)))
      case None =>
        IO.pure(None)
