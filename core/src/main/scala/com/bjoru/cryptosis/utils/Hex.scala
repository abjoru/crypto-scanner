package com.bjoru.cryptosis.utils

import java.math.BigInteger

object Hex:

  def hex2Bytes(hex: String): Array[Byte] =
    val slice = removePrefixIfExist(hex)
    slice.toSeq.sliding(2, 2).toArray.map(v => Integer.parseInt(v.toString, 16).toByte)

  def bytes2Hex(bytes: Array[Byte], withPrefix: Boolean = false): String =
    val hex = bytes.map("%02x" format _).mkString
    if withPrefix then s"0x${hex}" else hex

  def hex2BigInt(hex: String): BigInt = 
    BigInt(new BigInteger(hex2Bytes(hex)))

  def bigInt2Hex(value: BigInt, withPrefix: Boolean = false): String =
    val hex = bytes2Hex(value.toByteArray).replaceFirst("^0+(?!$)", "")
    if withPrefix then s"0x${hex}" else hex

  def hex2Int(hex: String): Int =
    Integer.parseInt(removePrefixIfExist(hex), 16)

  def int2Hex(value: Int, withPrefix: Boolean = false): String =
    if withPrefix then s"0x${value.toHexString}" else value.toHexString

  def hex2Long(hex: String): Long = 
    java.lang.Long.parseLong(removePrefixIfExist(hex), 16)

  def long2Hex(value: Long, withPrefix: Boolean = false): String =
    if withPrefix then s"0x${value.toHexString}" else value.toHexString

  private def removePrefixIfExist(hex: String): String =
    if hex.startsWith("0x") || hex.startsWith("0X")
      then hex.drop(2)
      else hex
