package com.bjoru.cryptosis.types

import cats.{Eq, Show}

import io.circe.*

import org.http4s.Uri.Path.SegmentEncoder

opaque type Symbol = String

object Symbol:

  given Eq[Symbol] = Eq.instance((a, b) => a.toLowerCase == b.toLowerCase)

  given Show[Symbol] = Show.show(_.upper)

  given Encoder[Symbol] = Encoder.encodeString

  given Decoder[Symbol] = Decoder.decodeString.map(apply)

  given SegmentEncoder[Symbol] = SegmentEncoder.stringSegmentEncoder

  given Conversion[Symbol, String] = v => v

  // Bluechips
  val Btc   = Symbol("btc")
  val Bch   = Symbol("bch")
  val Bsv   = Symbol("bsv")
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
  val Op    = Symbol("op")
  val Xdai  = Symbol("xdai")
  val Celo  = Symbol("celo")
  val Movr  = Symbol("movr")
  val Nrg   = Symbol("nrg")
  val Cro   = Symbol("cro")
  val Near  = Symbol("near")

  val Unknown = Symbol("<unknown>")

  extension (s: Symbol)
    def lower: String = s.toLowerCase
    def upper: String = s.toUpperCase
    def cap: String   = s.capitalize
  
  def apply(str: String): Symbol = str.toLowerCase
