package com.bjoru.cscanner

import cats.Foldable
import cats.effect.*
import cats.implicits.given

import org.http4s.client.*
import org.http4s.ember.client.*

import com.bjoru.cryptos.api.CryptoApi
import com.bjoru.cryptos.types.*

object Main extends IOApp:

  val cfgDir = getXdgDirectory(Xdg.Config) </> "crypto-scanner"

  val clientR = EmberClientBuilder.default[IO].build

  def run(args: List[String]): IO[ExitCode] = 
    for wallets <- Wallet.loadWallets(cfgDir </> "wallets.yaml")
        apis    <- CryptoApi.loadApis(cfgDir)
        filled  <- clientR.use(syncWallets(wallets, apis))
        _       <- IO(filled.foreach(w => println(w.show)))
        _       <- IO(println("-------------------------------------"))
        _       <- IO(println(s"Portfolio Totals: $$${filled.foldLeft(0.0)(_ + _.valueUsd)}"))
    yield ExitCode.Success

  def syncWallets(wallets: Seq[Wallet], apis: Seq[CryptoApi])(client: Client[IO]): IO[Seq[Wallet]] =
    Foldable[Seq].foldM(apis, wallets) {
      case (wx, api) => api.syncWallets(wx)(client)
    }


    /*
  def scanChain(chain: Chain, wallets: Set[Wallet]) = chain match
    case Chain.Bitcoin   => btc.BitcoinApi(cfgDir).totals(cfgDir, wallets)
    case Chain.Ethereum  => eth.EthereumApi(cfgDir).totals(cfgDir, wallets)
    case Chain.Elrond    => egld.ElrondApi(cfgDir).totals(cfgDir, wallets)
    case Chain.Dogecoin  => doge.DogecoinApi(cfgDir).totals(cfgDir, wallets)
    case Chain.Solana    => sol.SolanaApi(cfgDir).totals(cfgDir, wallets)
    case Chain.Avalanche => avax.AvalancheApi(cfgDir).totals(cfgDir, wallets)
    case Chain.Binance   => bnb.BinanceApi(cfgDir).totals(cfgDir, wallets)
    case Chain.Fantom    => ftm.FantomApi(cfgDir).totals(cfgDir, wallets)
    case Chain.Harmony   => one.HarmonyApi(cfgDir).totals(cfgDir, wallets)
    case Chain.Polygon   => matic.PolygonApi(cfgDir).totals(cfgDir, wallets)
    //case _               => IO.raiseError(new Exception("Unsupported at the moment!"))

  private def formatOutput(chain: Chain, data: (Double, Seq[TokenBalance])): IO[Unit] = IO {
    println(s"Holdings for $chain:")
    data._2.map(_.show).foreach(println)
    println("------------------------")
    println(s"Total: $$${data._1}")
  }
  */
