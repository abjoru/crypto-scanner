package com.bjoru.cryptos.utils

import scala.util.{Try, Success}

def decodeQuantity(value: String): Try[BigInt] = value match
  case v if isHexValue(v) => Success(BigInt(value.substring(2), 16))
  case v                  => Try(BigInt(v))

private def isLongValue(v: String): Boolean = 
  Try(v.toLong).isSuccess

private def isHexValue(str: String): Boolean = str match
  case v if v.isEmpty                          => false
  case v if v.length < 3                       => false
  case v if !v.startsWith("0x")                => false
  case v if v.length > 3 && v.charAt(2) == '0' => false
  case _                                       => true
