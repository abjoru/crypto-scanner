package com.bjoru.cscanner.egld

import cats.syntax.show.given
import cats.effect.IO

import org.http4s.ember.client.*

import munit.CatsEffectSuite
import com.bjoru.cscanner.{*, given}
import com.bjoru.cscanner.types.{Wallet, Address}
import com.bjoru.cscanner.config.{loadWallets, loadTokens}

class ElrondApiTest extends CatsEffectSuite:

  val cfgDir = getXdgDirectory(Xdg.Config) </> "crypto-scanner"

  test("Query elrond token balances") {
    for ws <- loadWallets(cfgDir </> WALLETS_FILE).map(_.filter(_.chain == Chain.Elrond))
        rs <- ElrondApi(cfgDir).balances(ws.toSet)
        _  <- IO(rs.foreach(println))
    yield assert(rs.nonEmpty)
  }
