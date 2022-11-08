package com.bjoru.cryptosis

import cats.effect.IO
import cats.syntax.traverse.given
import cats.syntax.foldable.given

import org.http4s.client.Client

import com.bjoru.cryptosis.types.*
import com.bjoru.cryptosis.instances.given

object Providers:

  val FILTERS   = cryptosisDirectory(Xdg.Config) </> "token-filters.yaml"
  val PROVIDERS = cryptosisDirectory(Xdg.Config) </> "endpoints.yaml"

  def syncWallets(wallets: Seq[Wallet])(using Client[IO]): IO[Seq[SyncResponse]] =
    loadProviders.flatMap(_.traverse(_.update(wallets)))

  def syncAndUpdateWallets(wallets: Seq[Wallet])(using Client[IO]): SIO[Seq[Wallet]] = 
    for resp <- SIO.liftF(syncWallets(wallets))
        _     = resp.groupBy(_.provider).mapValues(_.size).foreach(println)
        resu <- resp.foldLeftM(wallets)((wx, r) => r.syncWallets(wx))
    yield resu

  private def inspectResponses(rx: Seq[SyncResponse]): IO[Unit] = IO {
    rx.foreach { resp =>
      resp.data.foreach { dt =>
        println(s"SyncData: ${dt.walletAddress} ${dt.key} ${dt.responseData.size} items")
      }
    }
  }

  private def loadProviders: IO[Seq[ProviderApi]] =
    for endpoints <- loadYaml[Map[ProviderName, Endpoint]](PROVIDERS)
    yield mkApis(endpoints)

  private def mkApis(endpoints: Map[ProviderName, Endpoint]) =
    val apis = endpoints.collect {
      case (ProviderName.BlockCypher, e) => providers.BlockCypher(e)
      case (ProviderName.Elrond, e)      => providers.ElrondApi(e)
      case (ProviderName.Solscan, e)     => providers.Solscan(e)
      case (ProviderName.CovalentHQ, e)  => providers.CovalentHQ(e)
      case (ProviderName.Zapper, e)      => providers.Zapper(e)
    }

    apis.toSeq
