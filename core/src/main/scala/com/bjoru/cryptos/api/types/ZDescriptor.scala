package com.bjoru.cryptos.api.types

import cats.Show
import cats.syntax.show.given

import io.circe.{Json, Decoder}

import com.bjoru.cryptos.types.*

final case class ZDescriptor(
  key:          String,
  address:      Address,
  tpe:          String,
  appId:        Option[String],
  balanceUSD:   Usd,
  metaType:     Option[String],
  network:      String,
  contractType: String,
  displayProps: Option[ZDisplayProps],
  breakdown:    Seq[ZBreakdown]
)

object ZDescriptor:

  given Show[ZDescriptor] = Show.show { d =>
    s"""key:          ${d.key}
       |address:      ${d.address}
       |type:         ${d.tpe}
       |appId:        ${d.appId.getOrElse("<unknown>")}
       |balanceUSD:   ${d.balanceUSD}
       |metaType:     ${d.metaType.getOrElse("<unknown>")}
       |contractType: ${d.contractType}
       |breakdown:
       |${d.breakdown.map(_.show).mkString("- ", "\n", "")}""".stripMargin
  }

  given Decoder[ZDescriptor] = Decoder.instance { c =>
    for key  <- c.downField("key").as[String]
        addr <- c.downField("address").as[Address]
        tpe  <- c.downField("type").as[String]
        aid  <- c.downField("appId").as[Option[String]]
        busd <- c.downField("balanceUSD").as[Usd]
        mtpe <- c.downField("metaType").as[Option[String]]
        netw <- c.downField("network").as[String]
        ctpe <- c.downField("contractType").as[String]
        dprp <- c.downField("displayProps").as[Option[ZDisplayProps]]
        bdwn <- c.downField("breakdown").as[Seq[ZBreakdown]]
    yield ZDescriptor(key, addr, tpe, aid, busd, mtpe, netw, ctpe, dprp, bdwn)
  }

  extension (d: ZDescriptor)
    def claims: Seq[(Symbol, Balance, Usd)] = 
      val maybes = d.breakdown.map { b =>
        b.context.map(c => (c.symbol, c.balance, c.price))
      }

      maybes.flatten
