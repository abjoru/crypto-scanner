package com.bjoru.cryptosis.abi

trait SolDecoder[T <: SolType]:
  def decode(bytes: Array[Byte], position: Int): (T, Int)

object SolDecoder:

  def instance[T](f: (Array[Byte], Int) => (T, Int)): SolDecoder[T] =
    new SolDecoder[T]:
      def decode(bytes: Array[Byte], position: Int) = f(bytes, position)
