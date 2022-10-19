package com.bjoru.cryptos.api.types

import io.circe.Decoder

import com.bjoru.cryptos.types.*

final case class ZTotal(
  key: String,
  tpe: String,
  network: String,
  balanceUSD: Usd
)

object ZTotal:

  given Decoder[ZTotal] = Decoder.instance { c =>
    for k <- c.downField("key").as[String]
        t <- c.downField("type").as[String]
        n <- c.downField("network").as[String]
        b <- c.downField("balanceUSD").as[Usd]
    yield ZTotal(k, t, n, b)
  }

  extension (t: ZTotal)
    def chain: Option[Chain] = Chain.fromString(t.network)
