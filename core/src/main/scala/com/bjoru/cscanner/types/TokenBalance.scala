package com.bjoru.cscanner.types

import cats.Show
import cats.syntax.show.given

import io.circe.*

import scala.math.BigDecimal.RoundingMode

final case class TokenBalance(
  token: Token,
  balance: BigDecimal
)

object TokenBalance:

  given Show[TokenBalance] = Show.show {
    case TokenBalance(t, b) =>
      s"${b.setScale(4, RoundingMode.HALF_UP)} ${t.symbol.show}"
  }
