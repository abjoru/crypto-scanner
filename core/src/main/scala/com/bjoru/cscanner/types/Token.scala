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

  extension (s: Symbol)
    def nonEmpty: Boolean = s.toString.size > 0

  def apply(str: String): Symbol = str.toUpperCase

final case class Token(
  symbol:   Symbol,
  name:     String,
  decimals: Int,
  contract: Option[Address],
  priceUsd: Option[Double] = None
) derives ConfigReader

object Token:

  val Btc  = Token(Symbol("BTC"), "Bitcoin", 8, None)
  val Eth  = Token(Symbol("ETH"), "Ethereum", 0, None)
  val Sol  = Token(Symbol("SOL"), "Solana", 8, None)
  val Doge = Token(Symbol("DOGE"), "Dogecoin", 8, None)

  extension (t: Token)
    def withPrice(price: Double): Token =
      t.copy(priceUsd = Some(price))
