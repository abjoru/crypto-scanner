package com.bjoru.cryptosis.types

import io.circe.*

import pureconfig.ConfigReader
import pureconfig.error.ExceptionThrown

import scala.util.Try

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
  case Polkadot  extends Chain(Symbol.Dot)
  case Cardano   extends Chain(Symbol.Ada)
  case Unknown   extends Chain(Symbol.Unknown)

object Chain:

  given Encoder[Chain] = Encoder.encodeString.contramap(_.str)

  given Decoder[Chain] = Decoder.decodeString.emapTry(fromString)

  given ConfigReader[Chain] = ConfigReader.fromString { str =>
    fromString(str).toEither.left.map(ExceptionThrown(_))
  }

  extension (c: Chain)
    def str: String = c.toString.toLowerCase

  def fromString(str: String): Try[Chain] =
    Try(Chain.valueOf(str.capitalize))
