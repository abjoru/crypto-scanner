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

  protected def sync(wallets: Seq[Wallet])(using Client[IO]): IO[SyncResponse] =
    for wx   <- IO.pure(wallets.filter(_.chain == Chain.Elrond))
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

  def withData(extras: Seq[SyncData]): SyncResponse =
    ElrondResponse(data ++ extras)

  def syncWallet(env: Env, wallet: Wallet)(using Client[IO]) =
    case SyncData(_, "bal", jsons) =>
      for toks <- jsons.traverse(_.as[(Token, Option[Price])]).toIO
          reg  <- env.registerWithPrice(toks)
      yield reg.tuple(tx => wallet.addBalances(tx: _*))

    case SyncData(_, "egld", Seq(json)) =>
      for tok <- env.bluechip(Chain.Elrond)
          reg <- env.registerToken(tok)
          res <- json.hcursor.downField("balance").as[String].toIO
          bal <- Balance.convert(reg.data.decimals, res).toIO
      yield reg.tuple(wallet.addBalances(_))
