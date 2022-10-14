package com.bjoru.cscanner

import cats.effect.IO

import com.bjoru.cscanner.{*, given}
import com.bjoru.cscanner.types.Symbol

import munit.CatsEffectSuite

class CoingeckoTest extends CatsEffectSuite:

  val cfgDir = getXdgDirectory(Xdg.Config) </> "crypto-scanner"

  test("Load price data for symbols") {
    for a <- Coingecko(cfgDir).priceFor(Set(Symbol("BTC"), Symbol("ETH"), Symbol("SOL")))
        _ <- IO(a.foreach(println))
    yield assert(a.nonEmpty)
  }
