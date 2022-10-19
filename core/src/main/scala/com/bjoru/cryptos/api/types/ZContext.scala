package com.bjoru.cryptos.api.types

import cats.Show
import cats.syntax.show.given

import io.circe.Decoder
import io.circe.generic.semiauto.*

import com.bjoru.cryptos.types.*

final case class ZContext(
  symbol:     Symbol,
  balance:    Balance,
  decimals:   Int,
  balanceRaw: BigInt,
  price:      Usd
)

object ZContext:

  given Show[ZContext] = Show.show { c =>
    s"${c.balance.show} ${c.symbol.show}"
  }

  given Decoder[ZContext] = deriveDecoder[ZContext]
