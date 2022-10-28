package com.bjoru.cryptosis.types

import com.bjoru.cryptosis.*

final case class Result[T](
  env:  Env,
  data: T
)

object Result:

  extension [T](r: Result[T])
    def tuple[U](f: T => U): (Env, U) = (r.env, f(r.data))

  def of[T](data: T)(using e: Env): Result[T] =
    Result(e, data)
