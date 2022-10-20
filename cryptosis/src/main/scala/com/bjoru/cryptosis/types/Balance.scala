package com.bjoru.cryptosis.types

import cats.Show

import java.text.NumberFormat

opaque type Balance = BigDecimal

object Balance:

  val numFormat = NumberFormat.getNumberInstance

  given Show[Balance] = Show.show(numFormat.format(_))

  extension (b: Balance)
    def isEmpty: Boolean = b == Zero
    def toBigDecimal: BigDecimal = b

  val Zero: Balance = BigDecimal(0.0)

  def apply(value: String): Balance = value match
    case v if isHexValue(v) => apply(BigInt(v.substring(2), 16))
    case v                  => BigDecimal(value)

  def apply(value: BigInt): Balance = BigDecimal(value)

  def fromRaw(value: BigInt | String, decimals: Int): Balance = value match
    case v: BigInt => BigDecimal(v) / math.pow(10, decimals)
    case v: String => BigDecimal(v) / math.pow(10, decimals)

  private def isHexValue(str: String): Boolean = str match
    case v if v.isEmpty                          => false
    case v if v.length < 3                       => false
    case v if !v.startsWith("0x")                => false
    case v if v.length > 3 && v.charAt(2) == '0' => false
    case _                                       => true
