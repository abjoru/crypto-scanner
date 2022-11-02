package com.bjoru.cryptosis.abi

import cats.Show

import com.bjoru.cryptosis.utils.Hex
import com.bjoru.cryptosis.abi.generated.*

import java.math.BigInteger

val maxBitLength = 256
val maxByteLength = 32

def paddedBytes(value: BigInt): Array[Byte] =
  val padded = if value.signum == -1 then 0xff else 0x00
  Array.fill[Byte](maxByteLength)(padded.toByte)

// Marker interface
trait SolType

enum Solidity extends SolType:
  case Address(value: Array[Byte])
  case Bool(value: Boolean)
  case DynamicArray[T <: SolType](value: Seq[T])

object Solidity:

  given Show[Solidity] = Show.show {
    case Address(v) => v.toString
    case Bool(v)    => v.toString
    case DynamicArray(v) => v.mkString("[", ", ", "]")
  }

  ////////////////
  // TypeInfo's //
  ////////////////

  given TypeInfo[Address] = TypeInfo.instance("address", true)

  given TypeInfo[Bool] = TypeInfo.instance("bool", true)

  given [T <: SolType](using e: TypeInfo[T]): TypeInfo[DynamicArray[T]] =
    TypeInfo.instance(s"${e.name}[]", false)

  /////////////////////////
  // Encoders & Decoders //
  /////////////////////////

  given (using e: SolEncoder[Uint160]): SolEncoder[Address] = 
    SolEncoder.instance(t => e.encode(Uint160(BigInt(new BigInteger(t.value)))))

  given (using e: SolDecoder[Uint160]): SolDecoder[Address] =
    SolDecoder.instance { (a, b) =>
      val (result, consumed) = e.decode(a, b)
      (Address(result.value.toByteArray), consumed)
    }

  given (using e: SolEncoder[Uint8]): SolEncoder[Bool] = SolEncoder.instance {
    case Bool(true)  => e.encode(Uint8(BigInt(1)))
    case Bool(false) => e.encode(Uint8(BigInt(0)))
  }

  given (using e: SolDecoder[Uint8]): SolDecoder[Bool] = SolDecoder.instance { (a, b) =>
    val (result, consumed) = e.decode(a, b)
    if result.value.toInt == 1
      then (Bool(true), consumed)
      else (Bool(false), consumed)
  }

  //////////////////
  // Constructors //
  //////////////////

  def addressOf(str: String): Address = Address(Hex.hex2Bytes(str))

  def boolOf(str: String): Bool = str.toLowerCase match
    case "true"  => Bool(true)
    case "false" => Bool(false)
    case _ => throw Exception("only construct Bool from 'true' or 'false'")
