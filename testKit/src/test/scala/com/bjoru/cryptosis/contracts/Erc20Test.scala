package com.bjoru.cryptosis.contracts

import org.http4s.implicits.uri

import com.bjoru.cryptosis.types.*

import munit.CatsEffectSuite

class Erc20Test extends CatsEffectSuite:

  given Web3Env = Web3Env.build(
    fromAddress = Address.unsafeFromString("0x983873529f95132bd1812a3b52c98fb271d2f679"),
    nodeUri = uri"https://spring-newest-hill.discover.quiknode.pro/280fe979b48353d43de34e68f1e832d1a72c03fc/"
  )

  test("Get symbol of token") {
    val uosToken = Address.unsafeFromString("0xd13c7342e1ef687c5ad21b27c2b65d772cab5c8c")
    val result   = Erc20.symbolOf(uosToken)

    assertIO(result, Symbol("uos"))
  }
