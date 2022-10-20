package com.bjoru.cryptosis.types

import io.circe.*

import pureconfig.ConfigReader
import pureconfig.error.ExceptionThrown

import scala.util.{Try, Success, Failure}

opaque type Address = String

object Address:

  given Encoder[Address] = Encoder.encodeString.contramap(_.str)

  given Decoder[Address] = Decoder.decodeString.emapTry(fromString)

  given ConfigReader[Address] = 
    ConfigReader.fromString(v => fromString(v).toEither.left.map(ExceptionThrown(_)))

  extension (a: Address)
    def str: String = a

  def fromString(str: String): Try[Address] =
    if str.nonEmpty
      then Success(str)
      else Failure(new Exception("address cannot be empty!"))
