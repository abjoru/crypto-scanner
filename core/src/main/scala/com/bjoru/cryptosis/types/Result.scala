package com.bjoru.cryptosis.types

import com.bjoru.cryptosis.*

final case class Result[T](
  state:  State,
  data: T
)

object Result:

  extension [T](r: Result[T])
    def tuple[U](f: T => U): (State, U) = (r.state, f(r.data))

  def of[T](data: T)(using s: State): Result[T] =
    Result(s, data)
