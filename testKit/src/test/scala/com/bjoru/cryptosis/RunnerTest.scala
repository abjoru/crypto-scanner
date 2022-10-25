package com.bjoru.cryptosis

import cats.effect.IO
import cats.syntax.show.given
import cats.syntax.traverse.given

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
        w2 <- clientR.use(c => ProviderApi.syncWallets(cfgDir, ws)(using c))
        w3 <- clientR.use(c => w2.traverse(_.priceTokens(using c)))
        _  <- putStrLn("-----------------------------------------")
        _  <- IO(w3.foreach(w => println(s"${w.name}: ${w.valueUsd.map(_.show).getOrElse(Price.Zero.show)}")))
        _  <- putStrLn("-----------------------------------------")
        to  = w3.map(_.valueUsd.getOrElse(Price.Zero)).reduce(_ + _)
        _  <- putStrLn(s"Total: ${to.show}")
    yield assert(w3.nonEmpty)
  }
