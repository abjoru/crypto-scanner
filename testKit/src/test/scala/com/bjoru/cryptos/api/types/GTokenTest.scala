package com.bjoru.cryptos.api.types

import cats.effect.IO
import cats.implicits.given

import io.circe.*
import io.circe.parser.parse

import com.bjoru.cryptos.api.TokenRegistry
import com.bjoru.cryptos.types.*
import com.bjoru.cryptos.utils.{*, given}

import scala.io.Source

import munit.CatsEffectSuite

class GTokenTest extends CatsEffectSuite:

  val cfgDir = getXdgDirectory(Xdg.Config) </> "crypto-scanner"

  test("Load tokens") {
    for r <- GToken.loadGTokens(cfgDir </> "tokens.yaml")
    yield assert(r.nonEmpty)
  }

  test("Find tokens") {
    for r <- IO.pure(TokenRegistry(cfgDir))
        t <- r.findBySymbol(Symbol.Sol)
        _ <- IO(println(t))
    yield assert(true)
  }
