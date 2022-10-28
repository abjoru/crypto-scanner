package com.bjoru.cryptosis

import cats.Foldable
import cats.effect.*
import cats.implicits.given

import org.http4s.ember.client.*

import com.bjoru.cryptosis.types.*
import com.bjoru.cryptosis.views.*

import scala.concurrent.duration.*

object Main extends IOApp:

  val cfgDir = cryptosisDirectory(Xdg.Config)

  val clientR = EmberClientBuilder.default[IO]
                                  .withTimeout(2.minutes)
                                  .build

  def run(args: List[String]): IO[ExitCode] =
    for env <- IO.pure(CryptoEnv.coingecko)
        _   <- putStrLn("Loading wallets...")
        ws1 <- Wallet.loadWallets
        _   <- putStrLn("Syncing providers...")
        ws2 <- clientR.use(Providers.syncAndUpdateWallets(ws1)(using _).run(env))
        _   <- putStrLn("Syncing prices...")
        e2  <- clientR.use(ws2._1.syncPrices(using _))
        _   <- putStrLn("----------------------------------------------")
        _   <- TokenListView.render(e2, ws2._2)
        _   <- putStrLn("----------------------------------------------")
        _   <- DefiListView.render(e2, ws2._2)
        _   <- putStrLn("----------------------------------------------")
        _   <- PortfolioTotalsView.render(e2, ws2._2)
    yield ExitCode.Success
