package com.bjoru.cryptosis.types

import pureconfig.ConfigReader
import pureconfig.error.ExceptionThrown

import scala.util.Try

enum Chain:
  case Bitcoin
  case Ethereum
  case Solana
  case Elrond
  case Binance
  case Avalanche
  case Fantom
  case Polygon
  case Harmony
  case Dogecoin
  case Polkadot
  case Cardano

object Chain:

  given ConfigReader[Chain] = ConfigReader.fromString { str =>
    fromString(str).toEither.left.map(ExceptionThrown(_))
  }

  extension (c: Chain)
    def str: String = c.toString.toLowerCase

  def fromString(str: String): Try[Chain] =
    Try(Chain.valueOf(str.capitalize))
