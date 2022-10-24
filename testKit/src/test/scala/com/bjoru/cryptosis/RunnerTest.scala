package com.bjoru.cryptosis

import cats.effect.IO
import cats.syntax.show.given

import org.http4s.ember.client.*
import org.http4s.implicits.uri

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*

import munit.CatsEffectSuite

import scala.concurrent.duration.given

class RunnerTest extends CatsEffectSuite:

  val cfgDir = getXdgDirectory(Xdg.Config) </> "cryptosis"
  val chDir  = getXdgDirectory(Xdg.Cache) </> "cryptosis"

  val clientR = EmberClientBuilder.default[IO]
                  .withIdleTimeInPool(5.minutes)
                  .build

  test("Sync wallet") {
    for ws <- Wallet.loadWallets(cfgDir </> "wallets.yaml")
        ev <- clientR.use(Env.loadEnv(chDir))
        w2 <- clientR.use(ProviderApi.syncWallets(cfgDir, ws, _).run(ev))
        tx  = w2._2.foldLeft(Seq.empty[Token])(_ ++ _.unpricedTokens)
        e2 <- clientR.use(GeckoPricer.fetchPrices(tx, _)(w2._1))
        w3  = w2._2.map(_.priceTokens(e2))
        //_  <- IO(w3.foreach(_.balancesNoDust.foreach {
                //case (_, t: Token) => println(t.show)
                //case (_, d: Defi)  => println(d.show)
              //}))
        _  <- putStrLn("-----------------------------------------")
        _  <- IO(w3.foreach(w => println(s"${w.name}: ${w.valueUsd.map(_.show).getOrElse(Price.Zero.show)}")))
        _  <- putStrLn("-----------------------------------------")
        to  = w3.map(_.valueUsd.getOrElse(Price.Zero)).reduce(_ + _)
        _  <- putStrLn(s"Total: ${to.show}")
    yield assert(w3.nonEmpty)
  }
