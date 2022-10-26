package com.bjoru.cryptosis.types

import cats.Show

import io.circe.*
import pureconfig.ConfigReader

import scala.util.Try

import java.text.NumberFormat

opaque type Price = BigDecimal

object Price:

  given Show[Price] = Show.show(currencyF.format(_))

  given Encoder[Price] = Encoder.encodeBigDecimal

  given Decoder[Price] = Decoder.decodeBigDecimal

  given Conversion[Price, BigDecimal] = v => v

  given Numeric[Price] = new Numeric[Price] {
    val bd = Numeric.BigDecimalIsFractional
    def plus(x: Price, y: Price): Price = bd.plus(x, y)
    def minus(x: Price, y: Price): Price = bd.minus(x, y)
    def times(x: Price, y: Price): Price = bd.times(x, y)
    def negate(x: Price): Price = bd.negate(x)
    def fromInt(x: Int): Price = bd.fromInt(x)
    def parseString(str: String): Option[Price] = bd.parseString(str)
    def toInt(x: Price): Int = bd.toInt(x)
    def toLong(x: Price): Long = bd.toLong(x)
    def toFloat(x: Price): Float = bd.toFloat(x)
    def toDouble(x: Price): Double = bd.toDouble(x)
    def compare(x: Price, y: Price): Int = bd.compare(x, y)
  }

  extension (usd: Price)
    def +(other: BigDecimal): Price = usd + other
    def *(other: BigDecimal): Price = usd * other
    def asBigDecimal: BigDecimal = usd

  private val currencyF = NumberFormat.getCurrencyInstance

  val Zero = apply(0)

  def apply(value: Int): Price = BigDecimal(value)

  def apply(value: Double): Price = BigDecimal(value)

  def apply(value: BigDecimal): Price = value

  def fromString(value: String): Try[Price] = Try(BigDecimal(value))
