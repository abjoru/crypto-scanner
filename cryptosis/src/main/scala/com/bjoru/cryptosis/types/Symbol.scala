package com.bjoru.cryptosis.types

import io.circe.*

opaque type Symbol = String

object Symbol:

  given Encoder[Symbol] = Encoder.encodeString.contramap(_.lower)

  given Decoder[Symbol] = Decoder.decodeString.map(apply)

  val Btc = apply("btc")

  extension (s: Symbol)
    def lower: String = s.toLowerCase
    def upper: String = s.toUpperCase

  def apply(str: String): Symbol = str
