package com.bjoru.cryptosis.providers

import cats.effect.IO
import cats.syntax.traverse.*
import cats.syntax.foldable.*

import io.circe.*
import io.circe.syntax.*

import org.http4s.client.Client
import org.http4s.circe.*
import org.http4s.circe.CirceEntityDecoder.given

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*

class Solscan(ep: Endpoint) extends ProviderApi("solscan"):

  val chainFilter: Wallet => Boolean = _.chain == Chain.Solana

  protected def sync(wallets: Seq[Wallet])(using Client[cats.effect.IO]): IO[SyncResponse] =
    for ws <- IO.pure(wallets.filter(chainFilter))
        r1 <- ws.traverse(solBalance)
        r2 <- ws.traverse(tokBalance)
        r3 <- ws.traverse(staking)
    yield SolscanResponse(r1 ++ r2)

  def solBalance(wallet: Wallet)(using client: Client[IO]): IO[SyncData] =
    for uri <- IO.pure(ep.uri / "account" / wallet.address)
        jsn <- client.expect[Json](uri)
    yield SyncData(wallet.address, "sol", Seq(jsn))

  def tokBalance(wallet: Wallet)(using client: Client[IO]): IO[SyncData] = 
    for uri <- IO.pure(ep.uri / "account" / "tokens" +? ("account" -> wallet.address))
        res <- client.expect[Seq[Json]](uri)
    yield SyncData(wallet.address, "bal", res)

  def staking(wallet: Wallet)(using client: Client[IO]): IO[SyncData] =
    for uri <- IO.pure(ep.uri / "account" / "stakeAccounts" +? ("account", wallet.address))
        res <- client.expect[Json](uri)
    yield SyncData(wallet.address, "stake", Seq(res))

class SolscanResponse(val data: Seq[SyncData]) extends SyncResponse:

  given Decoder[Token] = Decoder.instance { hc =>
    for a <- hc.downField("tokenAddress").as[Address]
        b <- hc.downField("tokenAmount").downField("amount").as[String]
        c <- hc.downField("tokenAmount").downField("decimals").as[Int]
        d <- hc.downField("tokenName").as[String]
        e <- hc.downField("tokenSymbol").as[Symbol]
        f <- Balance.convert(c, b).toCirce(hc)
    yield Token(d, d, e, Chain.Solana, Some(a), c, f)
  }

  val LAMPORTS_PR_SOL = BigDecimal(1000000000.0)

  def withData(extras: Seq[SyncData]): SyncResponse = 
    SolscanResponse(data ++ extras)

  def syncWallets(wallets: Seq[Wallet])(env: Env)(using Client[IO]): IO[(Env, Seq[Wallet])] =
    // FIXME this won't work! we silently filtering wallets here!!
    wallets.foldLeftM(env -> Seq.empty[Wallet])(processWallet)

  def processWallet(acc: (Env, Seq[Wallet]), wallet: Wallet)(using Client[IO]): IO[(Env, Seq[Wallet])] =
    data.filter(_.walletAddress == wallet.address).foldLeftM(acc) {
      case ((env, wx), d) => parseData(env, wallet, d).map(v => v._1 -> (wx :+ v._2))
    }

  def parseData(env: Env, w: Wallet, d: SyncData)(using Client[IO]): IO[(Env, Wallet)] = d match
    case SyncData(_, "sol", Seq(json)) =>
      for sol <- env.bluechip(Chain.Solana)
          lam <- IO.fromEither(json.hcursor.downField("lamports").as[BigDecimal])
          bal  = Balance(lam / LAMPORTS_PR_SOL)
      yield env -> w.addBalances(sol.withBalance(bal))
    case SyncData(_, "bal", jsons) =>
      for sym <- IO.fromEither(jsons.traverse(j => j.hcursor.downField("tokenSymbol").as[Option[Symbol]].map(j -> _)))
          res <- IO.fromEither(sym.filter(_._2.isDefined).traverse(_._1.as[Token]))
          slv <- env.register(res: _*)
      yield slv.env -> w.addBalances(slv.data: _*)
    case SyncData(_, "stake", Seq(json)) =>
      IO.pure(json.hcursor.keys.map(_.head)).flatMap(parseStake(env, w, json))
    case _ => IO.pure(env -> w)

  def parseStake(
    env: Env, 
    w: Wallet, 
    json: Json
  )(key: Option[String])(using Client[IO]): IO[(Env, Wallet)] = key match
    case Some(k) =>
      for a <- IO.fromEither(json.hcursor.downField(k).as[Json])
          b <- IO.fromEither(a.hcursor.downField("amount").as[String])
          c <- env.bluechip(Chain.Solana)
          d  = Balance(BigDecimal(b) / LAMPORTS_PR_SOL)
          e <- env.register(c)
          f  = c.withBalance(d)
      yield e.env -> w.addBalances(Defi.Stake("solscan-sol-staking", "Solana Staking", Chain.Solana, Seq(f)))
    case None => IO.pure(env -> w)
