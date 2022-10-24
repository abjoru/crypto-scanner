package com.bjoru.cryptosis

import cats.data.StateT
import cats.effect.IO
import cats.syntax.traverse.given
import cats.syntax.foldable.given

import org.http4s.client.*

import com.bjoru.cryptosis.types.*

trait ProviderApi:

  val supportedChains: Seq[Chain]

  def syncWallets(wallets: Seq[Wallet], client: Client[IO])(env: Env): IO[(Env, Seq[Wallet])] = 
    val (supported, unsupported) = wallets.partition(w => supportedChains.contains(w.chain))
    doSync(supported, env)(using client).map(kv => kv._1 -> (kv._2 ++ unsupported))

  protected def doSync(wallets: Seq[Wallet], env: Env)(using Client[IO]): IO[(Env, Seq[Wallet])]

  def foreachWallet(env: Env, wallets: Seq[Wallet])(
    f: (Env, Wallet) => IO[(Env, Wallet)]
  )(using Client[IO]): IO[(Env, Seq[Wallet])] =
    wallets.foldLeftM(env -> Seq.empty[Wallet]) {
      case ((e, ws), w) => f(e, w).map(v => v._1 -> (ws :+ v._2))
    }

object ProviderApi:

  def syncWallets(cfgDir: FilePath, wallets: Seq[Wallet], client: Client[IO]): StateT[IO, Env, Seq[Wallet]] =
    for pv <- StateT.liftF(loadApis(cfgDir))
        rs <- StateT.apply(syncAllProviders(pv, wallets, client))
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
    wallets:   Seq[Wallet], 
    client:    Client[IO]
  )(env: Env): IO[(Env, Seq[Wallet])] = providers.foldLeftM(env -> wallets) {
    case ((env, wallets), api) => api.syncWallets(wallets, client)(env)
  }
