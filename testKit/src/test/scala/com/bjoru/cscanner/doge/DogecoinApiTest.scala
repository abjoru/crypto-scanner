package com.bjoru.cscanner.doge

import cats.syntax.show.given
import cats.effect.IO

import org.http4s.ember.client.*

import munit.CatsEffectSuite
import com.bjoru.cscanner.{*, given}
import com.bjoru.cscanner.types.{Wallet, Address}
import com.bjoru.cscanner.config.{loadWallets, loadTokens}

class DogecoinApiTest extends CatsEffectSuite:

  val cfgDir  = getXdgDirectory(Xdg.Config) </> "crypto-scanner"
  val wallets = loadWallets(cfgDir </> "wallets.yaml").map(_.filter(_.chain == Chain.Dogecoin))

  test("Query dogecoin wallet balance") {
    for ws <- wallets
        rs <- DogecoinApi(cfgDir).balances(ws.toSet)
        _  <- IO(rs.foreach(println))
    yield assert(rs.nonEmpty)
  }
