package com.bjoru.cryptosis.types

import cats.Show

import io.circe.*

import pureconfig.ConfigReader
import pureconfig.error.ExceptionThrown

import scala.util.{Try, Success, Failure}

opaque type Address = String

object Address:

  given Show[Address] = Show.fromToString[Address]

  given Encoder[Address] = Encoder.encodeString

  given Decoder[Address] = Decoder.decodeString

  given ConfigReader[Address] = ConfigReader.stringConfigReader

  given Conversion[Address, String] = v => v

  def fromString(str: Address): Try[Address] =
    if str.nonEmpty
      then Success(str)
      else Failure(Exception("address cannot be empty!"))
