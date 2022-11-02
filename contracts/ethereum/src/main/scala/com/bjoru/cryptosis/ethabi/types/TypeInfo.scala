package com.bjoru.cryptosis.ethabi.types

trait TypeInfo[+T <: SolType]:
  extension (t: T)
    def name: String
    def isStatic: Boolean
    def encode[U >: T](value: U): Array[Byte]
    def decode(bytes: Array[Byte], position: Int): (T, Int)
