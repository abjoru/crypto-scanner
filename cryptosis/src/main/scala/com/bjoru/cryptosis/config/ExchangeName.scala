package com.bjoru.cryptosis.config

import pureconfig.ConfigReader
import pureconfig.error.ExceptionThrown

import scala.util.{Try, Success, Failure}

enum ExchangeName:
  case BinanceUS

object ExchangeName:

  given ConfigReader[ExchangeName] = ConfigReader.fromString { str =>
    Try(ExchangeName.valueOf(str)).toEither.left.map(ExceptionThrown(_))
  }
