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

  extension (usd: Price)
    def +(other: BigDecimal): Price = usd + other
    def *(other: BigDecimal): Price = usd * other

  private val currencyF = NumberFormat.getCurrencyInstance

  val Zero = apply(0)

  def apply(value: Int): Price = BigDecimal(value)

  def apply(value: Double): Price = BigDecimal(value)

  def apply(value: BigDecimal): Price = value

  def fromString(value: String): Try[Price] = Try(BigDecimal(value))
