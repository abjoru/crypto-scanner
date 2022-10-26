package com.bjoru.cryptosis.types

import pureconfig.ConfigReader
import pureconfig.error.ExceptionThrown

import scala.util.{Try, Success, Failure}

enum ProviderName:
  case Zapper
  case Zerion
  case CovalentHQ
  case BlockCypher
  case QuickNode
  case Elrond
  case Solscan

object ProviderName:

  given ConfigReader[ProviderName] = ConfigReader.fromString { str =>
    Try(ProviderName.valueOf(str)) match
      case Success(p) => Right(p)
      case Failure(e) => Left(ExceptionThrown(e))
  }
