package com.bjoru.cryptosis

import cats.effect.IO
import cats.syntax.foldable.given
import cats.syntax.traverse.given

import org.http4s.client.Client

import com.bjoru.cryptosis.types.*
import com.bjoru.cryptosis.instances.given

import scala.concurrent.duration.*

trait ProviderApi(name: String):

  final def update(wallets: Seq[Wallet])(using Client[IO]): IO[SyncResponse] =
    for status <- checkCaches(wallets)
        fetch  <- sync(status._2)
        _      <- cacheData(fetch)
    yield fetch.withData(status._1)
    

  protected def sync(wallets: Seq[Wallet])(using Client[IO]): IO[SyncResponse]

  private def checkCaches(wallets: Seq[Wallet]): IO[(Seq[SyncData], Seq[Wallet])] =
    wallets.foldLeftM(Seq.empty[SyncData], Seq.empty[Wallet]) {
      case ((data, wx), w) if fileName(w.address).expired(4.hours) =>
        IO.pure(data -> (wx :+ w))
      case ((data, wx), w) if fileName(w.address).exists =>
        loadJson[SyncData](fileName(w.address)).map(d => (data :+ d) -> wx)
      case ((data, wx), w) =>
        IO.pure(data -> (wx :+ w))
    }

  private def fileName(w: Address): FilePath =
    cryptosisDirectory(Xdg.Cache) </> s"$name-$w.json"

  private def cacheData(resp: SyncResponse): IO[Unit] = 
    resp.data.traverse(d => saveJson(fileName(d.walletAddress), d)) >> IO.unit
