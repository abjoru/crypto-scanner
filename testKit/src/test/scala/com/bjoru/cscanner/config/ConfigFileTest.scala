package com.bjoru.cscanner.config

import com.bjoru.cscanner.{*, given}

import munit.CatsEffectSuite

class ConfigFileTest extends CatsEffectSuite:

  val cfgDir = getXdgDirectory(Xdg.Config) </> "crypto-scanner"

  test("Load token list") {
    val io = loadTokens(cfgDir </> "tokens.yaml")
    io.map { v =>
      println(v)
      assert(v.nonEmpty)
    }
  }
