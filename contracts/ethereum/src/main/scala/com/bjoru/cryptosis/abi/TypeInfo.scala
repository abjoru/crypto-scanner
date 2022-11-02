package com.bjoru.cryptosis.abi

trait TypeInfo[T <: SolType]:
  def name: String
  def static: Boolean

object TypeInfo:

  def instance[T <: SolType](nameValue: String, staticValue: Boolean): TypeInfo[T] =
    new TypeInfo[T]:
      def name = nameValue
      def static = staticValue
