package com.bjoru.cryptosis.types

import com.bjoru.cryptosis.*

final case class Result[T](
  env:  Env,
  data: T
)

object Result:

  def of[T](data: T)(using e: Env): Result[T] =
    Result(e, data)
