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

class ElrondApi(ep: Endpoint) extends ProviderApi("elrond"):

  val chainFilter: Wallet => Boolean = _.chain == Chain.Elrond

  protected def sync(wallets: Seq[Wallet])(using Client[IO]): IO[SyncResponse] =
    for wx   <- IO.pure(wallets.filter(chainFilter))
        egld <- wx.traverse(egld)
        bal  <- wx.traverse(balance)
    yield ElrondResponse(egld ++ bal)

  def egld(wallet: Wallet)(using client: Client[IO]): IO[SyncData] =
    for url <- IO.pure(ep.uri / "accounts" / wallet.address)
        jsn <- client.expect[Json](url)
    yield SyncData(wallet.address, "egld", Seq(jsn))

  def balance(wallet: Wallet)(using client: Client[IO]): IO[SyncData] =
    for url <- IO.pure(ep.uri / "accounts" / wallet.address / "tokens")
        jsn <- client.expect[Seq[Json]](url)
    yield SyncData(wallet.address, "bal", jsn)

class ElrondResponse(val data: Seq[SyncData]) extends SyncResponse:

  given Decoder[(Token, Option[Price])] = Decoder.instance { hc =>
    for a <- hc.downField("name").as[String]
        b <- hc.downField("ticker").as[Symbol]
        c <- hc.downField("decimals").as[Int]
        d <- hc.downField("balance").as[String]
        e <- hc.downField("valueUsd").as[Option[Price]]
        p <- Balance.convert(c, d).toCirce(hc)
    yield (Token(a.toLowerCase, a, b, Chain.Elrond, None, c, p), e)
  }

  def withData(extras: Seq[SyncData]): SyncResponse =
    ElrondResponse(data ++ extras)

  def syncWallets(wallets: Seq[Wallet])(env: Env)(using Client[IO]): IO[(Env, Seq[Wallet])] =
    wallets.foldLeftM(env -> Seq.empty[Wallet]) {
      case ((e2, wx), w) => parseSyncData(e2, w).map(v => v._1 -> (wx :+ v._2))
    }

  def parseSyncData(env: Env, wallet: Wallet)(using Client[IO]): IO[(Env, Wallet)] =
    data.filter(_.walletAddress == wallet.address).foldLeftM(env -> wallet) {
      case ((e2, w), SyncData(_, "bal", jsons)) =>
        for toks <- IO.fromEither(jsons.traverse(_.as[(Token, Option[Price])]))
            res  <- e2.registerWithPrice(toks: _*)
        yield res._1 -> wallet.addBalances(res._2: _*)
      case ((e2, w), SyncData(_, "egld", Seq(json))) =>
        for egld <- e2.bluechip(Chain.Elrond)
            bal  <- IO.fromEither(json.hcursor.downField("balance").as[String])
            res  <- IO.fromTry(Balance.convert(egld.decimals, bal))
        yield e2 -> wallet.addBalances(egld.withBalance(res))
      case (acc, _) => IO.pure(acc)
    }

  def parseToken(json: Json) = json.as[(Token, Option[Price])]
