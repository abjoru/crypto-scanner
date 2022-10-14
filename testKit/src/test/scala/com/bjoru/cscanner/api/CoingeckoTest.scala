package com.bjoru.cscanner.api

import cats.effect.IO

import com.bjoru.cscanner.{*, given}
import com.bjoru.cscanner.types.*

import munit.CatsEffectSuite

class CoingeckoTest extends CatsEffectSuite:

  import Balance.*

  val cfgDir = getXdgDirectory(Xdg.Config) </> "crypto-scanner"

  val tbs: Seq[TokenBalance] = Seq(TokenBalance(Token.Btc, BigDecimal(0.0)))

  test("Load price data for symbols") {
    for a <- Coingecko(cfgDir).priceFor(tbs)
        _ <- IO(a.foreach(println))
    yield assert(a.nonEmpty)
  }
