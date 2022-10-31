package com.bjoru.cryptosis

import cats.Foldable
import cats.effect.*
import cats.implicits.given

import org.http4s.ember.client.*

import com.bjoru.cryptosis.types.*
import com.bjoru.cryptosis.views.*
import com.bjoru.cryptosis.oracles.CoingeckoOracle

import scala.concurrent.duration.*

object Main extends IOApp:

  val cfgDir   = cryptosisDirectory(Xdg.Config)
  val cacheDir = cryptosisDirectory(Xdg.Cache)

  val clientR = EmberClientBuilder.default[IO]
                                  .withTimeout(2.minutes)
                                  .build

  def mkState: IO[State] = clientR.use { client =>
    State(
      tokensFile = cfgDir </> "tokens.json",
      unknownTokensFile = cfgDir </> "unknown-tokens.json",
      cachedOracleTokensFile = cacheDir </> "coingecko-tokens.json",
      oracle = new CoingeckoOracle
    )(using client)
  }

  def run(args: List[String]): IO[ExitCode] = 
    for st  <- mkState
        _   <- putStrLn("Loading wallets...")
        ws1 <- Wallet.loadWallets
        _   <- putStrLn("Syncing providers...")
        ws2 <- clientR.use(Providers.syncAndUpdateWallets(ws1)(using _).run(st))
        _   <- putStrLn("Syncing prices...")
        e2  <- clientR.use(Env.syncPrices(using _).run(ws2._1))
        _   <- putStrLn("----------------------------------------------")
        _   <- TokenListView.render(ws2._2).run(e2._1)
        _   <- putStrLn("----------------------------------------------")
        _   <- DefiListView.render(ws2._2).run(e2._1)
        _   <- putStrLn("----------------------------------------------")
        _   <- PortfolioTotalsView.render(ws2._2).run(e2._1)
        _   <- putStrLn("----------------------------------------------")
        _   <- Env.saveAll.run(e2._1)
    yield ExitCode.Success
