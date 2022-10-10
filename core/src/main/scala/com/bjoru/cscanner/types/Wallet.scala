package com.bjoru.cscanner.types

import pureconfig.*
import pureconfig.generic.derivation.default.*

final case class Wallet(
  name: String,
  address: Address
) derives ConfigReader
