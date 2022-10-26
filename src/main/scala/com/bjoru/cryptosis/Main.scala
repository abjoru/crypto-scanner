package com.bjoru.cryptosis

import cats.Foldable
import cats.effect.*
import cats.implicits.given

import org.http4s.ember.client.*

import com.bjoru.cryptosis.types.*

import scala.concurrent.duration.*

object Main extends IOApp:

  val cfgDir = getXdgDirectory(Xdg.Config) </> "cryptosis"
  val chFile = getXdgDirectory(Xdg.State) </> "cryptosis" </> "token-cache.json"

  val clientR = EmberClientBuilder.default[IO]
                                  .withTimeout(2.minutes)
                                  .build

  def run(args: List[String]): IO[ExitCode] = 
    for wallets <- Wallet.loadWallets(cfgDir </> "wallets.yaml")
        synched <- clientR.use(ProviderApi.syncWallets(cfgDir, wallets)(using _))
        priced  <- clientR.use(c => synched.traverse(_.priceTokens(using c)))
        _       <- outputWallets(priced)
    yield ExitCode.Success

  def outputWallets(wallets: Seq[Wallet]): IO[Unit] =
    for balances <- IO.pure(wallets.foldLeft(Seq.empty[Token | Defi])(_ ++ _.balances.values))
        _        <- putStrLn("Assets:")
        _        <- putStrLn("---------------------------------------------------------")
        _        <- printBalances(balances)
        _        <- putStrLn("---------------------------------------------------------")
        _        <- printTotals(wallets)
    yield ()

  def printBalances(items: Seq[Token | Defi]): IO[Unit] = IO {
    items.foreach {
      case t: Token => println(t.show)
      case d: Defi  => println(s">>> ${d.show}")
    }
  }

  def printTotals(wallets: Seq[Wallet]): IO[Unit] = IO {
    val values = wallets.traverse(_.valueUsd)
    val totals = values.map(_.reduce(_ + _)).getOrElse(Price.Zero)
    println(s"Totals: ${totals.show}")
  }
