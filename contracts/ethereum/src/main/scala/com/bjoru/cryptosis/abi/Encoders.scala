package com.bjoru.cryptosis.abi

trait SolEncoder[T <: SolType]:
  def encode(value: T): Array[Byte]

object SolEncoder:

  def instance[T](f: T => Array[Byte]): SolEncoder[T] =
    new SolEncoder[T]:
      def encode(value: T) = f(value)
