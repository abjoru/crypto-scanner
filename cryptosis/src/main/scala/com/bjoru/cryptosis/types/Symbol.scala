package com.bjoru.cryptosis.types

opaque type Symbol = String

object Symbol:

  extension (s: Symbol)
    def lower: String = s.toLowerCase
    def upper: String = s.toUpperCase

  def apply(str: String): Symbol = str
