package com.bjoru.cryptosis.ethabi.types

import java.math.BigInteger

import com.bjoru.cryptosis.ethabi.utils.Hex

final class Address(val value: Array[Byte]) extension SolType:
  assert(value.length == 20)

  override def toString = Hex.bytes2Hex(value, withPrefix = true)

object Address:

  val empty = Address(Array.fill[Byte](20)(0))

  def apply(bytes: Array[Byte]): Address = Address(bytes)
