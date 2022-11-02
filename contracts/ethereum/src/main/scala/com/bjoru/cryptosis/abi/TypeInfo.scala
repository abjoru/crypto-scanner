package com.bjoru.cryptosis.abi

trait TypeInfo[+T <: Solidity]:
  extension (t: T)
    def name: String
    def isStatic: Boolean
    def encode[U >: T](value: U): Array[Byte]
    def decode(bytes: Array[Byte], position: Int): (T, Int)
