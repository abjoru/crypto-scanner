package com.bjoru.cryptosis.ethabi.types

val maxBitLength = 256
val maxByteLength = 32

def paddedBytes(value: BigInt): Array[Byte] =
  val paddedValue = if value.signum == -1 then 0xff else 0x00
  Array.fill[Byte](maxByteLength)(paddedValue.toByte)
