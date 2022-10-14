package com.bjoru.cscanner

import cats.effect.*
import cats.syntax.show.given

import com.bjoru.cscanner.{*, given}
import com.bjoru.cscanner.types.*
import com.bjoru.cscanner.config.loadWallets

object Main extends IOApp:

  import Balance.*

  val cfgDir = getXdgDirectory(Xdg.Config) </> "crypto-scanner"

  def run(args: List[String]): IO[ExitCode] = 
    args.headOption.flatMap(Chain.fromString) match 
      case Some(chain) => 
        for wx <- loadWallets(cfgDir </> WALLETS_FILE).map(_.filter(_.chain == chain))
            rs <- scanChain(chain, wx.toSet)
            _  <- formatOutput(chain, rs)
        yield ExitCode.Success
      case None =>
        IO(println(s"Invalid chain: ${args.mkString}")).as(ExitCode.Error)

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
