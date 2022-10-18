package com.bjoru.cryptos.types

import cats.Show

import pureconfig.ConfigReader

import com.bjoru.cryptos.instances.*

opaque type Balance = BigDecimal

object Balance:

  given Show[Balance] = Show.show(v => numberFormatter.format(v))

  given Show[Option[Balance]] = Show.show(v => numberFormatter.format(v.getOrElse(Zero)))

  given ConfigReader[Balance] = ConfigReader.fromString(v => Right(apply(v)))

  extension (b: Balance)
    def +(other: Balance): Balance = b + other
    def *(other: BigDecimal): BigDecimal = b * other
    def pow10(d: Int): Balance = b / math.pow(10, d)
    def toBD: BigDecimal = b
    def isZero: Boolean = b == Zero

  val Zero: Balance = apply(0.0)

  def apply(value: Double): Balance = BigDecimal(value)

  def apply(value: BigInt): Balance = BigDecimal(value)

  def apply(value: String): Balance = BigDecimal(value)
