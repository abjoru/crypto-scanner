package com.bjoru.cryptosis.types

import cats.Show

import io.circe.*

opaque type Symbol = String

object Symbol:

  given Show[Symbol] = Show.show(_.upper)

  given Encoder[Symbol] = Encoder.encodeString.contramap(_.lower)

  given Decoder[Symbol] = Decoder.decodeString.map(apply)

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
  val Dot   = Symbol("dot")
  val Ada   = Symbol("ada")

  val Unknown = Symbol("<unknown>")

  extension (s: Symbol)
    def lower: String = s.toLowerCase
    def upper: String = s.toUpperCase

  def apply(str: String): Symbol = str.toLowerCase