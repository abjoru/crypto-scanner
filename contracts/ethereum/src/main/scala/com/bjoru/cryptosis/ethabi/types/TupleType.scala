package com.bjoru.cryptosis.ethabi.types

import scala.collection.mutable

trait TupleType extends SolType:
  def toSeq: Seq[SolType]

object TupleType:

  def encode(typeInfos: Seq[TypeInfo[SolType]], encodedValues: Seq[Array[Byte]]): Array[Byte] = 
    assert(typeInfos.length == encodedValues.length)

    val (staticLength, dynamicLength) = typeInfos.zip(encodedValues).foldLeft(0, 0) {
      case ((sl, dl), typeInfo, encoded) =>
        if typeInfo.isStatic
          then (sl + encoded.length, dl)
          else (sl + 32, dl + encoded.length)
    }

    val bytes = Array.fill[Byte](staticLength + dynamicLength)(0)

    val (staticOffset, dynamicOffset) = typeInfos.zip(encodedValues).foldLeft(0, staticLength) {
      case ((so, dyo), (typeInfo, encoded)) =>
        if typeInfo.isStatic
          then
            Array.copy(encoded, 0, bytes, so, encoded.length)
            (so + encoded.length, dyo)
          else
            val doe = TypeInfo[Uint256].encode(Uint256(BigInt(dyo)))
            Array.copy(doe, 0, bytes, so, 32)
            Array.copy(encoded, 0, bytes, dyo, encoded.length)
            so + 32
            (so, dyo + encoded.length)
    }

    bytes
