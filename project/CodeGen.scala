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
       |  given SolEncoder[Bytes$n] = SolEncoder.instance { t =>
       |    StaticBytes.encode(v.value, $n)
       |  }
       |
       |  given SolDecoder[Bytes$n] = SolDecoder.instance { (a, b) =>
       |    (Bytes$n(StaticBytes.decode(a, $n, b)), 32)
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
       |final class Int${n}(val value: BigInt) extends SolType:
       |  assert(value.bitLength <= $n)
       |  override def toString = value.toString
       |
       |object Int${n}:
       |
       |  given TypeInfo[Int$n] with
       |    extension (i: Int$n)
       |      def name = "int$n"
       |      def isStatic = true
       |      def encode[U >: Int$n](value: U) = IntType.encode(value.asInstanceOf[Int$n].value)
       |      def decode(bytes: Array[Byte], position: Int) = (Int$n(IntType.decode(bytes, $n, position)), 32)
       |
       |  def apply(value: BigInt): Int$n = new Int$n(value)
       |  def fromString(str: String): Int$n = apply(BigInt(str))
       |""".stripMargin

  def genUintN(n: Int): String = 
    s"""package com.bjoru.cryptosis.abi.generated
       |
       |import com.bjoru.cryptosis.abi.*
       |
       |final class Uint$n(val value: BigInt) extends SolType:
       |  assert(value.bitLength <= $n)
       |  override def toString = value.toString
       |
       |object Uint$n:
       |
       |  given TypeInfo[Uint$n] with
       |    extension (i: Uint$n)
       |      def name = "uint$n"
       |      def isStatic = true
       |      def encode[U >: Uint$n](value: U) = UintType.encode(value.asInstanceOf[Uint$n].value)
       |      def decode(bytes: Array[Byte], position: Int) = (Uint$n(UintType.decode(bytes, $n, position)), 32)
       |
       |  def apply(value: BigInt): Uint$n = new Uint$n(value)
       |  def fromString(str: String): Uint$n = apply(BigInt(str))
       |""".stripMargin
}
