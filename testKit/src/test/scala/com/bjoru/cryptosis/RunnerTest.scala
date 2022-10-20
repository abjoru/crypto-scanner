package com.bjoru.cryptosis

import cats.effect.IO

import org.http4s.ember.client.*
import org.http4s.implicits.uri

import com.bjoru.cryptosis.types.*

import munit.CatsEffectSuite

class RunnerTest extends CatsEffectSuite:

  val cfgDir = getXdgDirectory(Xdg.Config) </> "cryptosis"
  val chDir  = getXdgDirectory(Xdg.Cache) </> "cryptosis"

  val clientR = EmberClientBuilder.default[IO].build

  test("Sync wallet") {
    for ws <- Wallet.loadWallets(cfgDir </> "wallets.yaml")
        ev <- clientR.use(Env.loadEnv(chDir))
        w2 <- clientR.use(ProviderApi.syncWallets(cfgDir, ws, _).runA(ev))
        _  <- IO(w2.foreach(w => println(s"${w.name}: ${w.valueUsd.toOption}")))
    yield assert(w2.nonEmpty)
  }
