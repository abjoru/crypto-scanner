package com.bjoru.cryptosis.abi

import java.math.BigInteger

trait SolDecoder[T <: SolType]:
  def decode(bytes: Array[Byte], position: Int): (T, Int)

object SolDecoder:

  def instance[T <: SolType](f: (Array[Byte], Int) => (T, Int)): SolDecoder[T] =
    new SolDecoder[T]:
      def decode(bytes: Array[Byte], position: Int) = f(bytes, position)

  def staticBytes(bytes: Array[Byte], length: Int, position: Int): Array[Byte] =
    val encoded = bytes.slice(position, position + 32)
    val result = Array.fill[Byte](length)(0)
    Array.copy(encoded, 0, result, 0, length)
    result

  def intType(bytes: Array[Byte], length: Int, position: Int): BigInt =
    val encoded = bytes.slice(position, position + 32)
    val byteLength = length >> 3
    val result = Array.fill[Byte](byteLength + 1)(0)
    result(0) = encoded(0)
    val offset = maxByteLength - byteLength
    Array.copy(encoded, offset, result, 1, byteLength)
    BigInt(new BigInteger(result))

  def uintType(bytes: Array[Byte], length: Int, position: Int): BigInt =
    val encoded = bytes.slice(position, position + 32)
    val byteLength = length >> 3
    val result = Array.fill[Byte](byteLength)(0)
    val offset = maxByteLength - byteLength
    Array.copy(encoded, offset, result, 0, byteLength)
    BigInt(new BigInteger(result))
