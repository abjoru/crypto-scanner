package com.bjoru.cryptosis.abi

import com.bjoru.cryptosis.abi.generated.Uint256

trait TupleType extends SolType:
  def toSeq: Seq[SolType]

object TupleType:

  def encode[T <: SolType](types: Seq[T], encodedValues: Seq[Array[Byte]]): Array[Byte] = ???
  /*
    assert(types.length == encodedValues.length)

    val (staticLength, dynamicLength) = types.zip(encodedValues).foldLeft((0, 0)) {
      case ((sl, dl), (typeInfo, encoded)) =>
        if typeInfo.static 
          then (sl + encoded.length, dl)
          else (sl + 32, dl + encoded.length)
    }

    val bytes = Array.fill[Byte](staticLength + dynamicLength)
    val (staticOffset, dynamicOffset) = types.zip(encodedValues).foldLeft((0, 0)) {
      case ((staticO, dynamicO), (typeInfo, encoded)) =>
        if typeInfo.static
          then 
            Array.copy(encoded, 0, bytes, staticO, encoded.length)
            (staticO + encoded.length, dynamicO)
          else
            val dynOffEnc = summon[SolEncoder[Uint256]].encode(Uint256(BigInt(dynamicO)))
            Array.copy(dynOffEnc, 0, bytes, staticO, 32)
            Array.copy(encoded, 0, bytes, dynamicO, encoded.length)
            (staticO + 32, dynamicO + encoded.length)
    }

    bytes
    */

  def decode[T <: SolType](bytes: Array[Byte], position: Int, types: Seq[SolDecoder[_ <: SolType]]): (Seq[T], Int) = ???
  /*
    val (staticOffset, totalConsumed, results) = types.foldLeft((0, 0, Seq.empty[T])) {
      case ((so, tc, res), typeInfo) if typeInfo.static =>
        val (result, consumed) = summon[SolDecoder[T]].decode(bytes, so + position)
        (so + consumed, tc + consumed, res :+ result)
      case ((so, tc, res), typeInfo) =>
        val (offset, offsetConsumed) = summon[SolDecoder[Uint256]].decode(bytes, so + position)
        val so2 = so + offsetConsumed
        val tc2 = tc + offsetConsumed
        val (result, resultConsumed) = summon[SolDecoder[T]].decode(bytes, offset.value.toInt + position)
        (so2, tc2 + resultConsumed, res :+ result)
    }

    assert(results.length == types.length)
    (results, totalConsumed)
    */
