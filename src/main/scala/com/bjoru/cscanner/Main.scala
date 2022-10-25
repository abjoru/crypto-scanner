package com.bjoru.cscanner

import cats.Foldable
import cats.effect.*
import cats.implicits.given

import org.http4s.client.*
import org.http4s.ember.client.*

import com.bjoru.cryptosis.*
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
    yield ()

  def printBalances(items: Seq[Token | Defi]): IO[Unit] = IO {
    items.foreach {
      case t: Token => println(t.show)
      case d: Defi  => println(d.show)
    }
  }

    /*
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

  def printPrices(wallets: Seq[Wallet]): IO[Seq[String]] = IO {
    for w <- wallets
        t <- w.tokensNoEmpty
        a  = t.contract.map(_.toString).getOrElse("")
        b  = t.balance.map(_.show).getOrElse("0")
        s  = s"${w.chain} ($a) $b ${t.symbol} ${t.valueUsd.show}"
    yield s //++ " $" ++ t.valueUsd.show
  }
  */
