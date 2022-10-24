package com.bjoru.cryptosis.types

import io.circe.*

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
  case Polkadot  extends Chain(Symbol.Dot)
  case Cardano   extends Chain(Symbol.Ada)
  case Optimism  extends Chain(Symbol.Op)
  case Gnosis    extends Chain(Symbol.Xdai)
  case Arbitrum  extends Chain(Symbol.Eth)
  case Celo      extends Chain(Symbol.Celo)
  case Moonriver extends Chain(Symbol.Movr)
  case Aurora    extends Chain(Symbol.Eth)
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
    Try(Chain.valueOf(str.capitalize)).orElse(fromCustomStr(str))

  private def fromCustomStr(str: String): Try[Chain] = str match
    case "binance-smart-chain" => Success(Chain.Binance)
    case _                     => Failure(Exception(s"Cannot decode '$str' as Chain!"))
