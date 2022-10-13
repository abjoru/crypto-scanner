package com.bjoru.cscanner.btc

import cats.syntax.show.given
import cats.effect.IO

import org.http4s.ember.client.*

import munit.CatsEffectSuite
import com.bjoru.cscanner.{*, given}
import com.bjoru.cscanner.types.{Wallet, Address}
import com.bjoru.cscanner.config.{loadWallets, loadTokens}

class BitcoinApiTest extends CatsEffectSuite:

  val cfgDir  = getXdgDirectory(Xdg.Config) </> "crypto-scanner"
  val wallets = loadWallets(cfgDir </> "wallets.yaml").map(_.filter(_.chain == Chain.Bitcoin))

  test("Query bitcoin wallet balance") {
    for ws <- wallets
        rs <- BitcoinApi(cfgDir).balances(ws.toSet)
        _  <- IO(rs.flatMap(_._2).foreach(println))
    yield assert(rs.nonEmpty)
  }