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

class ElrondApi(ep: Endpoint) extends ProviderApi(ProviderName.Elrond):

  protected def sync(wallets: Seq[Wallet])(using Client[IO]): IO[SyncResponse] =
    for wx   <- IO.pure(wallets.filter(_.chain == Chain.Elrond))
        _    <- putStrLn(f"$name%-15s: synchronizing wallets...")
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

class ElrondResponse(val data: Seq[SyncData]) extends FoldableSyncResponse:

  given Decoder[(Token, Option[Price])] = Decoder.instance { hc =>
    for a <- hc.downField("name").as[String]
        b <- hc.downField("ticker").as[Symbol]
        c <- hc.downField("decimals").as[Int]
        d <- hc.downField("balance").as[String]
        e <- hc.downField("valueUsd").as[Option[Price]]
        p <- Balance.convert(c, d).toCirce(hc)
    yield (Token(a.toLowerCase, a, b, Chain.Elrond, None, c, p), e)
  }

  val provider: ProviderName = ProviderName.Elrond

  def withData(extras: Seq[SyncData]): SyncResponse =
    ElrondResponse(data ++ extras)

  def syncWallet(state: State, wallet: Wallet)(using Client[IO]) =
    case SyncData(_, "bal", jsons) =>
      for toks <- jsons.traverse(_.as[(Token, Option[Price])]).toIO
          reg   = state.resolveAllWithPrice(toks.map(v => v._1 -> v._2.getOrElse(Price.Zero)))
      yield reg._1 -> wallet.addBalances(reg._2: _*)

    case SyncData(_, "egld", Seq(json)) =>
      for tok <- IO.pure(state.bluechip(Chain.Elrond))
          res <- json.hcursor.downField("balance").as[String].toIO
          bal <- Balance.convert(tok._2.decimals, res).toIO
      yield tok._1 -> wallet.addBalances(tok._2)
