package com.bjoru.cryptos.api

import cats.effect.IO

import org.http4s.ember.client.*
import org.http4s.implicits.uri

import com.bjoru.cryptos.types.*
import com.bjoru.cryptos.utils.{*, given}

import munit.CatsEffectSuite

class ZapperApiTest extends CatsEffectSuite:

  val cfgDir = getXdgDirectory(Xdg.Config) </> "crypto-scanner"

  val ep = Endpoint(uri"https://api.zapper.fi/v2", "0f877f33-f9b6-4f3a-94f3-bdb94fe70d16")

  val wallet = Wallet("TestWallet", Chain.Ethereum, Address("0xE020495acCA466EbB89F8f3aE60471a813563668"))

  val clientR = EmberClientBuilder.default[IO].build

  test("Get supported apps from Zapper") {
    for r <- clientR.use(ZapperApi(ep).balances(Set(wallet)))
        _ <- IO(println(r))
    yield assert(r.nonEmpty)
  }
