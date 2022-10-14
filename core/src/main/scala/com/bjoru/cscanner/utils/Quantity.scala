package com.bjoru.cscanner.utils

import com.bjoru.cscanner.types.Token

import scala.util.{Try, Success, Failure}

object Quantity:

  val HexPrefix  = "0x"
  val HexCharMap = "0123456789abcdef".toCharArray

  def decodeQuantity(token: Token, value: String): Try[BigDecimal] =
    decodeQuantity(value).map(n => BigDecimal(n) / math.pow(10, token.decimals))

  def decodeQuantity(token: Token, value: BigDecimal): Try[BigDecimal] =
    Success(value / math.pow(10, token.decimals))

  def decodeQuantity(value: String): Try[BigInt] = value match
    case v if isHexValue(v)  => Success(BigInt(value.substring(2), 16))
    case v                   => Try(BigInt(v))

  private def isLongValue(v: String): Boolean = 
    Try(v.toLong).isSuccess

  private def isHexValue(str: String): Boolean = str match
    case v if v.isEmpty                          => false
    case v if v.length < 3                       => false
    case v if !v.startsWith(HexPrefix)           => false
    case v if v.length > 3 && v.charAt(2) == '0' => false
    case _                                       => true
