package com.bjoru.cryptosis.abi

trait SolEncoder[T <: SolType]:
  def encode(value: T): Array[Byte]

object SolEncoder:

  def instance[T <: SolType](f: T => Array[Byte]): SolEncoder[T] =
    new SolEncoder[T]:
      def encode(value: T) = f(value)

  def staticBytes(value: Array[Byte], length: Int): Array[Byte] =
    val result = Array.fill[Byte](maxByteLength)(0)
    Array.copy(value, 0, result, 0, length)
    result

  def intType(value: BigInt): Array[Byte] =
    val encoded = value.toByteArray
    val result  = paddedBytes(value)
    Array.copy(encoded, 0, result, maxByteLength - encoded.length, encoded.length)
    result

  def uintType(value: BigInt): Array[Byte] = 
    val encoded = if (value.bitLength == maxBitLength) {
      val bytes = new Array[Byte](maxByteLength)
      Array.copy(value.toByteArray, 1, bytes, 0, maxByteLength)
      bytes
    } else value.toByteArray

    val result = Array.fill[Byte](maxByteLength)(0)
    Array.copy(encoded, 0, result, maxByteLength - encoded.length, encoded.length)
    result
