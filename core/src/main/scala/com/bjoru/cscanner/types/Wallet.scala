package com.bjoru.cscanner.types

import cats.Show

import pureconfig.*
import pureconfig.generic.derivation.default.*

import com.bjoru.cscanner.Chain

final case class Wallet(
  chain:   Chain,
  name:    String,
  address: Address
) derives ConfigReader

object Wallet:

  given Show[Wallet] = Show.show(v => s"${v.name} [${v.chain}]")
