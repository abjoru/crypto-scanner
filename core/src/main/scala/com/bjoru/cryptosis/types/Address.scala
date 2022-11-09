package com.bjoru.cryptosis.types

import cats.{Show, Eq}

import io.circe.*

import pureconfig.ConfigReader
import pureconfig.error.ExceptionThrown

import org.http4s.QueryParamEncoder
import org.http4s.Uri.Path.SegmentEncoder

import scala.util.{Try, Success, Failure}

opaque type Address = String

object Address:

  given Eq[Address] = Eq.instance((a, b) => a.toLowerCase == b.toLowerCase)

  given Show[Address] = Show.fromToString[Address]

  given Encoder[Address] = Encoder.encodeString

  given Decoder[Address] = Decoder.decodeString.emapTry(fromString)

  given SegmentEncoder[Address] = SegmentEncoder.stringSegmentEncoder

  given ConfigReader[Address] = ConfigReader.stringConfigReader

  given Conversion[Address, String] = v => v

  given QueryParamEncoder[Address] = QueryParamEncoder.stringQueryParamEncoder

  def fromString(str: String): Try[Address] =
    if str.nonEmpty
      then Success(str)
      else Failure(Exception("address cannot be empty!"))

  def unsafeFromString(str: String): Address = 
    fromString(str).get
