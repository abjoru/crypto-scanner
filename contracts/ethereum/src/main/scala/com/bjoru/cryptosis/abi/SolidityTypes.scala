package com.bjoru.cryptosis.abi

import cats.Show

import com.bjoru.cryptosis.utils.Hex
import com.bjoru.cryptosis.abi.generated.*

import java.math.BigInteger

// Marker interface
trait SolType:
  val name: String
  val static: Boolean

enum Solidity(name: String, static: Boolean) extends SolType:
  case Address(value: Array[Byte]) extends Solidity("address", true)
  case Bool(value: Boolean)        extends Solidity("bool", true)

object Solidity:

  given Show[Solidity] = Show.show {
    case Address(v) => v.toString
    case Bool(v)    => v.toString
  }

  given (using e: SolEncoder[Uint160]): SolEncoder[Address] = 
    SolEncoder.instance(t => e.encode(Uint160(BigInt(new BigInteger(t.value)))))

  given (using e: SolDecoder[Uint160]): SolDecoder[Address] =
    SolDecoder.instance { (a, b) =>
      val (result, consumed) = e.decode(a, b)
      (Address(result.value.toByteArray), consumed)
    }

  /*
  given (using e: TypeInfo[Uint160]): TypeInfo[Address] with
    extension (t: Address)
      def name = "address"
      def isStatic = true
      def encode[U >: Address](a: U) = 
        e.encode(Uint160(BigInt(new BigInteger(a.asInstanceOf[Address].value))))
      def decode(bytes: Array[Byte], position: Int) = 
        val (result, consumed) = e.decode(bytes, position)
        (Address(result.value.toByteArray), consumed)

  given (using e: TypeInfo[Uint8]): TypeInfo[Bool] with
    extension (t: Bool)
      def name = "bool"
      def isStatic = true
      def encode[U >: Bool](a: U) = 
        if a.asInstanceOf[Bool].value
          then e.encode(Uint8(BigInt(1)))
          else e.encode(Uint8(BigInt(0)))
      def decode(bytes: Array[Byte], position: Int) =
        val (result, consumed) = e.decode(bytes, position)
        if result.value.toInt == 1
          then (Bool(true), consumed)
          else (Bool(false), consumed)
          */

  def addressOf(str: String): Address = Address(Hex.hex2Bytes(str))

  def boolOf(str: String): Bool = str.toLowerCase match
    case "true"  => Bool(true)
    case "false" => Bool(false)
    case _ => throw Exception("only construct Bool from 'true' or 'false'")
