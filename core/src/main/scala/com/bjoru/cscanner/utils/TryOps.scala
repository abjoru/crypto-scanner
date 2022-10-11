package com.bjoru.cscanner.utils

import cats.effect.IO

import io.circe.{HCursor, DecodingFailure as DF}
import io.circe.Decoder.Result

import scala.util.Try

extension [T](t: Try[T])
  def circeResult(c: HCursor): Result[T] =
    t.toEither.left.map(e => DF.fromThrowable(e, c.history))

extension [T](io: IO[Try[T]])
  def circeResult(c: HCursor): IO[Result[T]] =
    io.map(_.circeResult(c))
