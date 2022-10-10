package com.bjoru.cscanner.types

import pureconfig.*
import pureconfig.generic.derivation.default.*

final case class Token(
  name:     String,
  symbol:   String,
  decimals: Int,
  contract: Option[Address],
) derives ConfigReader

object Token:

  val Eth = Token("Ethereum", "ETH", 0, None)
  val Btc = Token("Bitcoin", "BTC", 0, None)
