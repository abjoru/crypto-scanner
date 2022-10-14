package com.bjoru.cscanner.eth

import cats.syntax.show.given
import cats.effect.IO

import org.http4s.ember.client.*

import munit.CatsEffectSuite
import com.bjoru.cscanner.{*, given}
import com.bjoru.cscanner.types.{Wallet, Address}
import com.bjoru.cscanner.config.{loadWallets, loadTokens}

class EthereumApiTest extends CatsEffectSuite:

  val cfgDir  = getXdgDirectory(Xdg.Config) </> "crypto-scanner"
  val wallets = loadWallets(cfgDir </> "wallets.yaml").map(_.filter(_.chain == Chain.Ethereum))

  test("Query eth wallet balances") {
    for ws <- wallets
        rs <- EthereumApi(cfgDir).balances(ws.toSet)
        _  <- IO(rs.foreach(println))
    yield assert(rs.nonEmpty)
  }
