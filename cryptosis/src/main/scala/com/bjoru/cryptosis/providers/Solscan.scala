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

// FIXME don't know how to get SOL balance from this???
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

  protected def doSync(wallets: Seq[Wallet], env: Env)(using Client[IO]): IO[(Env, Seq[Wallet])] =
    for a <- foreachWallet(env, wallets)(solBalance)
        b <- foreachWallet(a._1, a._2)(balances)
        c <- foreachWallet(b._1, b._2)(staking)
    yield c

  private def solBalance(env: Env, wallet: Wallet)(using client: Client[IO]): IO[(Env, Wallet)] =
    for a <- IO.pure(ep.uri / "account" / wallet.address.str)
        b <- client.expect(a)(jsonOf[IO, Json])
        c <- IO.fromEither(b.hcursor.downField("lamports").as[BigDecimal])
        d <- IO.fromTry(env.bluechipToken(Chain.Solana))
        e  = d.withBalance(d.decimals, Balance.fromBigDecimal(c / LAMPORTS_PR_SOL))
    yield env -> wallet.addBalance(e)

  private def balances(env: Env, wallet: Wallet)(using client: Client[IO]): IO[(Env, Wallet)] = 
    for a <- IO.pure(ep.uri / "account" / "tokens" +? ("account" -> wallet.address.str))
        b <- client.expect(a)(jsonOf[IO, Seq[Json]])
        c <- IO.fromEither(b.traverse(j => j.hcursor.downField("tokenSymbol").as[Option[Symbol]].map(j -> _)))
        d <- IO.fromEither(c.filter(_._2.isDefined).traverse(_._1.as[Token]))
        e  = d.foldLeft(env -> Seq.empty[Token])(resolve)
    yield e._1 -> wallet.addBalances(e._2)

  private def staking(env: Env, wallet: Wallet)(using client: Client[IO]): IO[(Env, Wallet)] =
    for a <- IO.pure(ep.uri / "account" / "stakeAccounts" +? ("account" -> wallet.address.str))
        b <- client.expect(a)(jsonOf[IO, Json])
        c  = b.hcursor.keys.map(_.head)
        d <- parseStake(c, b, env)
    yield d._1 -> d._2.map(wallet.addBalance(_)).getOrElse(wallet)

  private def resolve(acc: (Env, Seq[Token]), t: Token): (Env, Seq[Token]) = 
    acc._1.resolveToken(t) match
      case Some(t2) => acc._1.updateToken(t2) -> (acc._2 :+ t2)
      case None     => acc._1.updateToken(t)  -> (acc._2 :+ t)

  private def parseStake(key: Option[String], json: Json, env: Env): IO[(Env, Option[Defi.Stake])] =
    key match
      case Some(k) =>
        for a <- IO.fromEither(json.hcursor.downField(k).as[Json])
            b <- IO.fromEither(a.hcursor.downField("amount").as[String])
            c <- IO.fromTry(env.bluechipToken(Chain.Solana))
            d  = c.withBalance(c.decimals, Balance.fromBigDecimal(BigDecimal(b) / LAMPORTS_PR_SOL))
        yield env -> Some(Defi.Stake("solscan-sol-staking", "Sol Staking", Chain.Solana, Seq(d)))
      case None =>
        IO.pure(env -> None)
