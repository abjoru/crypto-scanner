package com.bjoru.cryptosis.types

import cats.Show

import io.circe.Decoder
import pureconfig.ConfigReader

import scala.util.Try

import java.text.NumberFormat

opaque type Price = BigDecimal

object Price:

  given Show[Price] = Show.show(curFormat.format(_))

  given Decoder[Price] = Decoder.decodeBigDecimal.emap(p => Right(apply(p)))

  extension (usd: Price)
    def +(other: Price): Price = usd + other
    def *(other: BigDecimal): Price = usd * other
    def toBD: BigDecimal = usd

  val curFormat = NumberFormat.getCurrencyInstance

  val Zero = apply(0.0)

  def apply(value: Double): Price = BigDecimal(value)

  def apply(value: BigDecimal): Price = value

  def apply(value: String): Price = BigDecimal(value)
