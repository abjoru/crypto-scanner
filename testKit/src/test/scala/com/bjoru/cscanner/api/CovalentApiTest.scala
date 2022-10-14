package com.bjoru.cscanner.api

import cats.effect.IO

import org.http4s.ember.client.*

import com.bjoru.cscanner.{*, given}
import com.bjoru.cscanner.config.loadWallets
import com.bjoru.cscanner.types.Symbol

import munit.CatsEffectSuite

class CovalentApiTest extends CatsEffectSuite:

  val cfgDir = getXdgDirectory(Xdg.Config) </> "crypto-scanner"
  val wallets = loadWallets(cfgDir </> "wallets.yaml")

  val clientR = EmberClientBuilder.default[IO].build

  test("Get TraderJoe balances for Avalanche") {
    for w <- wallets.map(_.filter(_.chain == Chain.Avalanche))
        j <- clientR.use(CovalentApi(cfgDir, Chain.Avalanche).dexBalance(w.head, Dex.TraderJoe))
        _ <- IO(println(j.spaces2))
    yield assert(true)
  }
