package com.bjoru.cscanner

import cats.Foldable
import cats.effect.*
import cats.implicits.given

import org.http4s.client.*
import org.http4s.ember.client.*

import com.bjoru.cryptos.api.*
import com.bjoru.cryptos.types.*

object Main extends IOApp:

  val cfgDir = getXdgDirectory(Xdg.Config) </> "crypto-scanner"

  val clientR = EmberClientBuilder.default[IO].build

  def run(args: List[String]): IO[ExitCode] = 
    for wallets <- Wallet.loadWallets(cfgDir </> "wallets.yaml")
        apis    <- CryptoApi.loadApis(cfgDir)
        synced  <- clientR.use(syncWallets(wallets, apis))
        priced  <- clientR.use(checkPrices(synced, _))
        _       <- IO(priced.foreach(w => println(w.show)))
        _       <- IO(println("-------------------------------------"))
        _       <- IO(println(s"Portfolio Totals: $$${priced.foldLeft(0.0)(_ + _.valueUsd)}"))
    yield ExitCode.Success

  def syncWallets(wallets: Seq[Wallet], apis: Seq[CryptoApi])(client: Client[IO]): IO[Seq[Wallet]] =
    Foldable[Seq].foldM(apis, wallets) {
      case (wx, api) => api.syncWallets(wx)(client)
    }

  def checkPrices(wallets: Seq[Wallet], client: Client[IO]): IO[Seq[Wallet]] =
    val tokens = wallets.foldLeft(Seq.empty[Token])(_ ++ _.unpricedTokens).distinct

    if tokens.isEmpty then IO.pure(wallets) else 
      GeckoPriceApi(cfgDir).pricer(tokens)(client).map { pricer =>
        wallets.map(_.update(pricer.price))
      }