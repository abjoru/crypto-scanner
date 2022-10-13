package com.bjoru.cscanner.config

import com.bjoru.cscanner.{*, given}

import munit.CatsEffectSuite

class ConfigFileTest extends CatsEffectSuite:

  val cfgDir = getXdgDirectory(Xdg.Config) </> "crypto-scanner"

  test("Load token list") {
    loadTokens(cfgDir </> "tokens.yaml").map { v =>
      println(v)
      assert(v.nonEmpty)
    }
  }
