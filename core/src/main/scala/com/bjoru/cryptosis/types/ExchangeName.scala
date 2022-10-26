package com.bjoru.cryptosis.types

import pureconfig.ConfigReader
import pureconfig.error.ExceptionThrown

import scala.util.Try

enum ExchangeName:
  case BinanceUS

object ExchangeName:

  given ConfigReader[ExchangeName] = ConfigReader.fromString { str =>
    Try(ExchangeName.valueOf(str)).toEither.left.map(ExceptionThrown(_))
  }
