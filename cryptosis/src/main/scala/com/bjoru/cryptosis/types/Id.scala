package com.bjoru.cryptosis.types

import cats.Show
import cats.syntax.show.given

import io.circe.*

import com.bjoru.cryptosis.*

import scala.util.Success

import java.security.MessageDigest

opaque type Id = String

object Id:

  given Show[Id] = Show.fromToString[Id]

  given Encoder[Id] = Encoder.encodeString.contramap(v => v)

  given Decoder[Id] = Decoder.decodeString.emapTry(v => Success(v))

  extension (id: Id)
    def matches[T : Identity](other: T): Boolean = id == other.id

  val DIGEST_SHA256 = MessageDigest.getInstance("SHA-256")

  def create(data: String*): Id = 
    val input = data.reduce(_ ++ _)
    val bytes = DIGEST_SHA256.digest(input.getBytes("UTF-8"))
    bytes.map("%02x".format(_)).mkString
