package com.bjoru.cscanner.utils

import com.bjoru.cscanner.types.{Token, TokenBalance}

import scala.util.{Try, Success, Failure}

object Quantity:

  val HexPrefix  = "0x"
  val HexCharMap = "0123456789abcdef".toCharArray

  def decodeTokenQuantity(token: Token, value: String): Try[TokenBalance] = 
    decodeQuantity(value).map { n =>
      val bal = BigDecimal(n) / token.decimals
      TokenBalance(token, bal)
    }

  def decodeQuantity(value: String): Try[BigInt] = value match
    case v if isLongValue(v) => Try(BigInt(v.toLong))
    case v if isHexValue(v)  => Success(BigInt(value.substring(2), 16))
    case other               => Failure(new IllegalArgumentException(s"Invalid quantity format: $other"))

  private def isLongValue(v: String): Boolean = 
    Option(v.toLong).isDefined

  private def isHexValue(str: String): Boolean = str match
    case v if v.isEmpty                          => false
    case v if v.length < 3                       => false
    case v if !v.startsWith(HexPrefix)           => false
    case v if v.length > 3 && v.charAt(2) == '0' => false
    case _                                       => true
