package com.bjoru.cryptos.types

import cats.Show

import pureconfig.ConfigReader
import io.circe.Decoder

opaque type Address = String

object Address:

  given Show[Address] = Show.fromToString[Address]

  given Decoder[Address] = 
    Decoder.decodeString.emap(v => Right(apply(v)))

  given ConfigReader[Address] = 
    ConfigReader.fromString(v => Right(apply(v)))

  extension (a: Address)
    def str: String = a

  def apply(str: String): Address = str
