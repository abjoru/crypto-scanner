package com.bjoru.cryptos.types

import cats.Show

import io.circe.Decoder
import pureconfig.*
import pureconfig.generic.derivation.default.*

opaque type Symbol = String

object Symbol:

  given Show[Symbol] = Show.fromToString[Symbol]

  given Decoder[Symbol] = Decoder.decodeString.map(apply)

  given ConfigReader[Symbol] = ConfigReader.fromString(s => Right(apply(s)))

  // Bluechips
  val Btc   = Symbol("btc")
  val Eth   = Symbol("eth")
  val Sol   = Symbol("sol")
  val Egld  = Symbol("egld")
  val Bnb   = Symbol("bnb")
  val Avax  = Symbol("avax")
  val Ftm   = Symbol("ftm")
  val Matic = Symbol("matic")
  val One   = Symbol("one")
  val Doge  = Symbol("doge")

  extension (s: Symbol)
    def matches(str: String): Boolean =
      s.equalsIgnoreCase(str)

    def nonEmpty: Boolean = s.toString.size > 0

    def lower: String = s.toString.toLowerCase

  def apply(str: String): Symbol = str.toUpperCase
