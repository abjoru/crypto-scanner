package com.bjoru.cryptos.types

import cats.Show

import pureconfig.ConfigReader

import com.bjoru.cryptos.instances.*

opaque type Usd = BigDecimal

object Usd:

  given Show[Usd] = Show.show(v => currencyFormatter.format(v))

  given Show[Option[Usd]] = Show.show(v => currencyFormatter.format(v.getOrElse(Zero)))

  given ConfigReader[Usd] = ConfigReader.fromString(v => Right(apply(v)))

  extension (usd: Usd)
    def +(other: Usd): Usd = usd + other
    def *(other: BigDecimal): Usd = usd * other
    def toBD: BigDecimal = usd

  val Zero = apply(0.0)

  def apply(value: Double): Usd = BigDecimal(value)

  def apply(value: BigDecimal): Usd = value

  def apply(value: String): Usd = BigDecimal(value)
