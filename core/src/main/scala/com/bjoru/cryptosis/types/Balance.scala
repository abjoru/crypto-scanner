package com.bjoru.cryptosis.types

import cats.Show

import io.circe.Decoder

import scala.util.{Try, Success, Failure}

import java.text.NumberFormat

opaque type Balance = BigDecimal

object Balance:

  given Show[Balance] = Show.show(numberF.format(_))

  given Decoder[Balance] = Decoder.decodeBigDecimal

  given Conversion[Balance, BigDecimal] = v => v

  extension (b: Balance)
    def isEmpty: Boolean = b == Zero

  private lazy val numberF = NumberFormat.getNumberInstance

  val Zero: Balance = apply(0)

  def apply(value: Int): Balance = BigDecimal(value)

  def apply(value: Double): Balance = BigDecimal(value)

  def apply(value: BigInt): Balance = BigDecimal(value)

  def apply(value: BigDecimal): Balance = value

  def fromString(value: String): Try[Balance] = value match
    case v if isHex(v) => Success(apply(BigInt(v.substring(2), 16)))
    case v             => Try(BigDecimal(value))

  def convert(decimals: Int, value: BigInt | String): Try[Balance] = value match
    case v: BigInt => Success(BigDecimal(v) / math.pow(10, decimals))
    case v: String => Try(BigDecimal(v) / math.pow(10, decimals))

  private def isHex(str: String): Boolean = str match
    case v if v.isEmpty                          => false
    case v if v.length < 3                       => false
    case v if !v.startsWith("0x")                => false
    case v if v.length > 3 && v.charAt(2) == '0' => false
    case _                                       => true
