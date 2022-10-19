package com.bjoru.cryptos.api.types

import cats.Show
import cats.syntax.show.given

import io.circe.{Json, Decoder}

import com.bjoru.cryptos.types.*

final case class ZBreakdown(
  key:          String,
  appId:        Option[String],
  address:      Address,
  network:      String,
  balanceUSD:   Usd,
  metaType:     String,
  tpe:          String,
  contractType: String,
  breakdown:    Seq[ZBreakdown],
  context:      Option[ZContext],
  displayProps: ZDisplayProps
)

object ZBreakdown:

  given Show[ZBreakdown] = Show.show { b =>
    s"${b.context.map(_.show).getOrElse(b.key)} = ${b.balanceUSD.show}"
  }

  given Decoder[ZBreakdown] = Decoder.instance { c =>
    for key  <- c.downField("key").as[String]
        aid  <- c.downField("appId").as[Option[String]]
        addr <- c.downField("address").as[Address]
        netw <- c.downField("network").as[String]
        busd <- c.downField("balanceUSD").as[Usd]
        mtpe <- c.downField("metaType").as[String]
        tpe  <- c.downField("type").as[String]
        ctpe <- c.downField("contractType").as[String]
        bdwn <- c.downField("breakdown").as[Seq[ZBreakdown]]
        ctx  <- c.downField("context").as[Option[ZContext]]
        dprp <- c.downField("displayProps").as[ZDisplayProps]
    yield ZBreakdown(key, aid, addr, netw, busd, mtpe, tpe, ctpe, bdwn, ctx, dprp)
  }
