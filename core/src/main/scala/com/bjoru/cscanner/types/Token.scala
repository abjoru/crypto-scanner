package com.bjoru.cscanner.types

import cats.Show

import io.circe.Decoder

import pureconfig.*
import pureconfig.generic.derivation.default.*

opaque type Symbol = String

object Symbol:

  given Show[Symbol] = Show.fromToString[Symbol]

  given Decoder[Symbol] = Decoder.decodeString.map(apply)

  given ConfigReader[Symbol] = ConfigReader.fromString(s => Right(apply(s)))

  def apply(str: String): Symbol = str.toUpperCase

final case class Token(
  symbol:   Symbol,
  name:     String,
  decimals: Int,
  contract: Option[Address],
  priceUsd: Option[Double] = None
) derives ConfigReader

object Token:

  val Eth = Token(Symbol("ETH"), "Ethereum", 0, None)
  val Btc = Token(Symbol("BTC"), "Bitcoin", 8, None)
  val Doge = Token(Symbol("DOGE"), "Dogecoin", 8, None)
