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

  def syncAndUpdateWallets(wallets: Seq[Wallet])(using Client[IO]): SIO[Seq[Wallet]] = SIO { env =>
    syncWallets(wallets).flatMap { responses =>
      responses.foldLeftM(env -> wallets) {
        case ((e, acc), response) => response.syncWallets(acc)(e)
      }
    }
  }

  private def loadProviders: IO[Seq[ProviderApi]] =
    for endpoints <- loadYaml[Map[ProviderName, Endpoint]](PROVIDERS)
        filters   <- loadYaml[Seq[TokenFilter]](FILTERS).handleError(_ => Seq.empty)
    yield mkApis(endpoints, filters)

  private def mkApis(endpoints: Map[ProviderName, Endpoint], filters: Seq[TokenFilter]) =
    val apis = endpoints.collect {
      case (ProviderName.BlockCypher, e) => providers.BlockCypher(e)
    }

    apis.toSeq
