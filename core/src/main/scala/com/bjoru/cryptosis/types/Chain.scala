package com.bjoru.cryptosis.types

import cats.syntax.show.*

import io.circe.*

import pureconfig.ConfigReader
import pureconfig.error.ExceptionThrown

import scala.util.{Try, Success, Failure}

enum Chain(val symbol: Symbol):
  case Bitcoin      extends Chain(Symbol.Btc)
  case BitcoinCash  extends Chain(Symbol.Bch)
  case BitcoinSV    extends Chain(Symbol.Bsv)
  case Ethereum     extends Chain(Symbol.Eth)
  case Solana       extends Chain(Symbol.Sol)
  case Elrond       extends Chain(Symbol.Egld)
  case Binance      extends Chain(Symbol.Bnb)
  case Avalanche    extends Chain(Symbol.Avax)
  case Fantom       extends Chain(Symbol.Ftm)
  case Polygon      extends Chain(Symbol.Matic)
  case Harmony      extends Chain(Symbol.One)
  case Dogecoin     extends Chain(Symbol.Doge)
  case Polkadot     extends Chain(Symbol.Dot)
  case Cardano      extends Chain(Symbol.Ada)
  case Optimism     extends Chain(Symbol.Op)
  case Gnosis       extends Chain(Symbol.Xdai)
  case Arbitrum     extends Chain(Symbol.Eth)
  case Celo         extends Chain(Symbol.Celo)
  case Moonriver    extends Chain(Symbol.Movr)
  case Aurora       extends Chain(Symbol.Eth)
  case Energi       extends Chain(Symbol.Nrg)
  case Cronos       extends Chain(Symbol.Cro)
  case SmartBCH     extends Chain(Symbol.Bch)
  case NearProtocol extends Chain(Symbol.Near)
  case Unknown      extends Chain(Symbol.Unknown)

object Chain:

  given Ordering[Chain] = Ordering.by(_.toString)

  given Encoder[Chain] = Encoder.encodeString.contramap(_.toString.toLowerCase)

  given Decoder[Chain] = Decoder.decodeString.emapTry(fromString)

  given ConfigReader[Chain] = ConfigReader.fromString { str =>
    fromString(str).toEither.left.map(ExceptionThrown(_))
  }

  given Conversion[Chain, String] = _.toString.toLowerCase

  def fromString(str: String): Try[Chain] =
    Try(Chain.valueOf(str.capitalize)).orElse(fromCustom(str))

  private def fromCustom(str: String): Try[Chain] = str match
    case "binance-smart-chain" => Success(Chain.Binance)
    case "smartbch"            => Success(Chain.SmartBCH)
    case "nearprotocol"        => Success(Chain.NearProtocol)
    case "bitcoincash"         => Success(Chain.BitcoinCash)
    case "bitcoinsv"           => Success(Chain.BitcoinSV)
    case _                     => Failure(Exception(s"Cannot decode '$str' as Chain!"))
