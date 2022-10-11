package com.bjoru.cscanner.eth

import cats.syntax.show.given
import cats.effect.IO

import org.http4s.ember.client.*

import munit.CatsEffectSuite
import com.bjoru.cscanner.{*, given}
import com.bjoru.cscanner.types.{Wallet, Address}
import com.bjoru.cscanner.config.loadWallets

class QuickNodeApiTest extends CatsEffectSuite:

  val cfgDir = getXdgDirectory(Xdg.Config) </> "crypto-scanner"

  test("Query ETH wallet") {
    for wx  <- loadWallets(cfgDir </> "wallets.yaml")
        api <- QuickNodeApi.loadApi(cfgDir </> "quicknode.yaml")
        res <- EmberClientBuilder.default[IO].build.use(api.ethBalance(wx.head)(using _))
        _   <- IO(println(res.show))
    yield assert(res.balance > BigDecimal(0.0))
  }
