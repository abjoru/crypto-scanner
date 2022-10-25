package com.bjoru.cryptosis

import cats.data.StateT
import cats.effect.IO
import cats.syntax.traverse.given
import cats.syntax.foldable.given

import org.http4s.client.*

import com.bjoru.cryptosis.types.*

trait ProviderApi:

  val supportedChains: Seq[Chain]

  def syncWallets(wallets: Seq[Wallet])(using Client[IO]): IO[Seq[Wallet]] = 
    val (supported, unsupported) = wallets.partition(w => supportedChains.contains(w.chain))
    doSync(supported).map(_ ++ unsupported)

  protected def doSync(wallets: Seq[Wallet])(using Client[IO]): IO[Seq[Wallet]]

object ProviderApi:

  def syncWallets(cfgDir: FilePath, wallets: Seq[Wallet])(using Client[IO]): IO[Seq[Wallet]] =
    for pv <- loadApis(cfgDir)
        rs <- syncAllProviders(pv, wallets)
    yield rs

  def loadApis(cfgDir: FilePath): IO[Seq[ProviderApi]] = 
    for ep <- Endpoint.loadEndpoints(cfgDir </> "endpoints.yaml")
        tf <- TokenFilter.loadTokenFilters(cfgDir </> "token-filters.yaml")
    yield resolveApis(ep, tf)

  private def resolveApis(endpoints: Map[Provider, Endpoint], filters: Seq[TokenFilter]) =
    val apis = endpoints.collect {
      case (Provider.BlockCypher, e) => providers.BlockCypher(e)
      case (Provider.CovalentHQ, e)  => providers.CovalentHQ(e, filters)
      case (Provider.Solscan, e)     => providers.Solscan(e, filters)
      case (Provider.Elrond, e)      => providers.Elrond(e, filters)
      case (Provider.Zapper, e)      => providers.Zapper(e)
    }

    apis.toSeq

  private def syncAllProviders(
    providers: Seq[ProviderApi], 
    wallets:   Seq[Wallet]
  )(using Client[IO]): IO[Seq[Wallet]] = 
    providers.foldLeftM(wallets) {
      case (ws, api) => api.syncWallets(ws)
    }
