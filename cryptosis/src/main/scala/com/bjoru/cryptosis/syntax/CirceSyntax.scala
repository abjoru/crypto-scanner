package com.bjoru.cryptosis.syntax

import cats.effect.IO

import io.circe.*

object circe:

  extension (json: Json)
    def <\>(id: String): ACursor = json.hcursor.downField(id)
    def asIO[T](using Decoder[T]): IO[T] = IO.fromEither(json.as[T])

  extension (c: HCursor)
    def <\>(id: String): ACursor = c.downField(id)
    def asIO[T](using Decoder[T]): IO[T] = IO.fromEither(c.as[T])

  extension (c: ACursor)
    def <\>(id: String): ACursor = c.downField(id)
    def asIO[T](using Decoder[T]): IO[T] = IO.fromEither(c.as[T])
