package com.bjoru.cryptosis.config

import pureconfig.ConfigReader
import pureconfig.error.ExceptionThrown

import scala.util.{Try, Success, Failure}

enum Provider:
  case Zapper
  case Zerion
  case CovalentHQ
  case BlockCypher
  case QuickNode
  case Elrond
  case Solscan

object Provider:

  given ConfigReader[Provider] = ConfigReader.fromString { str =>
    Try(Provider.valueOf(str)) match
      case Success(p) => Right(p)
      case Failure(e) => Left(ExceptionThrown(e))
  }
