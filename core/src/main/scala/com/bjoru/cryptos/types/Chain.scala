package com.bjoru.cryptos.types

import cats.Show

import pureconfig.ConfigReader
import pureconfig.error.ExceptionThrown

import scala.util.{Try, Success, Failure}

enum Chain(val symbol: Symbol):
  case Bitcoin   extends Chain(Symbol.Btc)
  case Ethereum  extends Chain(Symbol.Eth)
  case Solana    extends Chain(Symbol.Sol)
  case Elrond    extends Chain(Symbol.Egld)
  case Binance   extends Chain(Symbol.Bnb)
  case Avalanche extends Chain(Symbol.Avax)
  case Fantom    extends Chain(Symbol.Ftm)
  case Polygon   extends Chain(Symbol.Matic)
  case Harmony   extends Chain(Symbol.One)
  case Dogecoin  extends Chain(Symbol.Doge)

object Chain:

  given Show[Chain] = Show.fromToString[Chain]

  given ConfigReader[Chain] = ConfigReader.fromString { str =>
    Try(Chain.valueOf(str)) match
      case Success(ch) => Right(ch)
      case Failure(er) => Left(ExceptionThrown(er))
  }

  extension (c: Chain)
    def isMultichain: Boolean = Seq(
      Chain.Ethereum,
      Chain.Binance,
      Chain.Avalanche,
      Chain.Fantom,
      Chain.Polygon,
      Chain.Harmony
    ).contains(c)

  def fromString(str: String): Option[Chain] =
    Try(Chain.valueOf(str.capitalize)).toOption
