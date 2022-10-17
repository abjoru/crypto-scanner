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
    Endpoint.loadAll(cfgDir </> "endpoints.yaml").map { ex =>
      val apis = ex.collect {
        case (Provider.CovalentHQ, e)  => CovalentApi(e)
        case (Provider.BlockCypher, e) => BlockCypherApi(e)
      }

      apis.toSeq
    }
