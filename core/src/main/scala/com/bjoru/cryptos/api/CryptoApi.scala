package com.bjoru.cryptos.api

import cats.effect.IO

import org.http4s.client.*

import com.bjoru.cryptos.utils.{*, given}
import com.bjoru.cryptos.types.*

import java.nio.file.Path

trait CryptoApi:

  val supportedChains: Seq[Chain]

  def syncWallets(wallets: Seq[Wallet])(client: Client[IO]): IO[Seq[Wallet]] =
    val (supported, unsupported) = wallets.partition(w => supportedChains.contains(w.chain))

    doSyncWallets(supported, client).map(_ ++ unsupported)

  protected def doSyncWallets(wallets: Seq[Wallet], client: Client[IO]): IO[Seq[Wallet]]

object CryptoApi:

  def loadApis(cfgDir: Path): IO[Seq[CryptoApi]] = 
    for ep <- Endpoint.loadAll(cfgDir </> "endpoints.yaml")
        tf <- TokenFilter.loadFilters(cfgDir </> "token-filter.yaml")
        rs  = resolveApis(ep, tf)
    yield rs

  def resolveApis(endpoints: Map[Provider, Endpoint], filters: Seq[TokenFilter]) =
    val apis = endpoints.collect {
      case (Provider.CovalentHQ, e)  => CovalentApi(e, filters)
      case (Provider.BlockCypher, e) => BlockCypherApi(e)
      case (Provider.Elrond, e)      => ElrondApi(e)
    }

    apis.toSeq
