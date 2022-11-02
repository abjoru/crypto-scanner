package com.bjoru.cryptosis

import sbt._

object CodeGen {

  def genSequence(baseDir: File, indices: Seq[Int], baseFilename: String)(gen: Int => String): Seq[File] = indices.map { i =>
    val file = baseDir / "com" / "bjoru" / "cryptosis" / "abi" / "generated" / s"$baseFilename$i.scala"
    if (file.exists) {
      file
    } else {
      IO.write(file, gen(i))
      file
    }
  }

  def genBytesN(n: Int): String = 
    s"""package com.bjoru.cryptosis.abi.generated
       |
       |import com.bjoru.cryptosis.abi.*
       |import com.bjoru.cryptosis.utils.Hex
       |
       |final class Bytes${n}(val value: Array[Byte]) extends SolType:
       |  assert(value.length <= $n)
       |  val name: String = "bytes$n"
       |  val static: Boolean = true
       |
       |object Bytes${n}:
       |
       |  given SolEncoder[Bytes$n] = 
       |    SolEncoder.instance(t => SolEncoder.staticBytes(t.value, $n))
       |
       |  given SolDecoder[Bytes$n] = SolDecoder.instance { (a, b) => 
       |    (Bytes$n(SolDecoder.staticBytes(a, $n, b)), 32)
       |  }
       |
       |  def apply(value: Array[Byte]): Bytes${n} = new Bytes${n}(value)
       |  def fromString(str: String): Bytes${n} = apply(Hex.hex2Bytes(str))
       |""".stripMargin

  def genIntN(n: Int): String =
    s"""package com.bjoru.cryptosis.abi.generated
       |
       |import com.bjoru.cryptosis.abi.*
       |
       |import java.math.BigInteger
       |
       |final class Int${n}(val value: BigInt) extends SolType:
       |  assert(value.bitLength <= $n)
       |  val name: String = "int$n"
       |  val static: Boolean = true
       |  override def toString = value.toString
       |
       |object Int${n}:
       |
       |  given SolEncoder[Int$n] = 
       |    SolEncoder.instance(t => SolEncoder.intType(t.value))
       |
       |  given SolDecoder[Int$n] = SolDecoder.instance { (a, b) => 
       |    (Int$n(SolDecoder.intType(a, $n, b)), 32)
       |  }
       |
       |  def apply(value: BigInt): Int$n = new Int$n(value)
       |  def fromString(str: String): Int$n = apply(BigInt(str))
       |""".stripMargin

  def genUintN(n: Int): String = 
    s"""package com.bjoru.cryptosis.abi.generated
       |
       |import com.bjoru.cryptosis.abi.*
       |
       |import java.math.BigInteger
       |
       |final class Uint$n(val value: BigInt) extends SolType:
       |  assert(value.bitLength <= $n)
       |  val name: String = "uint$n"
       |  val static: Boolean = true
       |  override def toString = value.toString
       |
       |object Uint$n:
       |
       |  given SolEncoder[Uint$n] = 
       |    SolEncoder.instance(t => SolEncoder.uintType(t.value))
       |
       |  given SolDecoder[Uint$n] = SolDecoder.instance { (a, b) => 
       |    (Uint$n(SolDecoder.uintType(a, $n, b)), 32)
       |  }
       |
       |  def apply(value: BigInt): Uint$n = new Uint$n(value)
       |  def fromString(str: String): Uint$n = apply(BigInt(str))
       |""".stripMargin
}
