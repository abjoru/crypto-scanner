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

class Solscan(ep: Endpoint) extends ProviderApi(ProviderName.Solscan):

  protected def sync(wallets: Seq[Wallet])(using Client[cats.effect.IO]): IO[SyncResponse] =
    for ws <- IO.pure(wallets.filter(_.chain == Chain.Solana))
        _  <- putStrLn(f"$name%-15s: synchronizing wallets...")
        r1 <- ws.traverse(solBalance)
        r2 <- ws.traverse(tokBalance)
        r3 <- ws.traverse(staking)
    yield SolscanResponse(r1 ++ r2 ++ r3)

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

class SolscanResponse(val data: Seq[SyncData]) extends FoldableSyncResponse:

  given Decoder[Token] = Decoder.instance { hc =>
    for a <- hc.downField("tokenAddress").as[Address]
        b <- hc.downField("tokenAmount").downField("amount").as[String]
        c <- hc.downField("tokenAmount").downField("decimals").as[Int]
        d <- hc.downField("tokenName").as[String]
        e <- hc.downField("tokenSymbol").as[Symbol]
        f <- Balance.convert(c, b).toCirce(hc)
    yield Token(d, d, e, Chain.Solana, Some(a), c, f)
  }

  val provider: ProviderName = ProviderName.Solscan

  val LAMPORTS_PR_SOL = BigDecimal(1000000000.0)

  def withData(extras: Seq[SyncData]): SyncResponse = 
    SolscanResponse(data ++ extras)

  def syncWallet(state: State, wallet: Wallet)(using Client[IO]) =
    case SyncData(_, "sol", Seq(json)) =>
      for tok <- IO.pure(state.bluechip(Chain.Solana))
          lam <- json.hcursor.downField("lamports").as[BigDecimal].toIO
          bal  = Balance(lam / LAMPORTS_PR_SOL)
      yield tok._1 -> wallet.addBalances(tok._2.withBalance(bal))

    case SyncData(_, "bal", jsons) =>
      for sym <- jsons.traverse(j => j.hcursor.downField("tokenSymbol").as[Option[Symbol]].map(j -> _)).toIO
          res <- sym.filter(_._2.isDefined).traverse(_._1.as[Token]).toIO
          reg  = state.resolveAll(res)
      yield reg._1 -> wallet.addBalances(reg._2: _*)

    case SyncData(_, "stake", Seq(json)) =>
      IO.pure(json.hcursor.keys.map(_.head)).flatMap(parseStake(state, wallet, json))

  def parseStake(
    state: State,
    w: Wallet, 
    json: Json
  )(key: Option[String])(using Client[IO]): IO[(State, Wallet)] = key match
    case Some(k) =>
      for a <- json.hcursor.downField(k).as[Json].toIO
          b <- a.hcursor.downField("amount").as[String].toIO
          c  = state.bluechip(Chain.Solana)
          d  = Balance(BigDecimal(b) / LAMPORTS_PR_SOL)
          f  = c._2.withBalance(d)
      yield c._1 -> w.addBalances(Defi.Stake("solscan-sol-staking", "Solana Staking", Chain.Solana, Seq(f)))
    case None => IO.pure(state -> w)
