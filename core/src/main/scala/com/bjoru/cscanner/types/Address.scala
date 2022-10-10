package com.bjoru.cscanner.types

import pureconfig.ConfigReader
import io.circe.{Decoder, Encoder}

opaque type Address = String

object Address:

  given ConfigReader[Address] = 
    ConfigReader.fromString(v => Right(apply(v)))

  given Encoder[Address] = Encoder.encodeString.contramap(_.stringValue)

  given Decoder[Address] = Decoder.decodeString.emap(v => Right(apply(v)))

  extension (a: Address)
    def stringValue: String = a

  def apply(str: String): Address = str
