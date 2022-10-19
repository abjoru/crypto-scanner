package com.bjoru.cryptos.api.types

import cats.implicits.{*, given}

import org.http4s.Uri

import io.circe.*
import io.circe.generic.semiauto.*

import com.bjoru.cryptos.instances.given

final case class ZDisplayProps(
  balanceDisplayMode: String,
  images:             Seq[Uri],
  label:              Option[String],
  secondaryLabel:     Option[String],
  tertiaryLabel:      Option[String],
  info:               Map[String, String],
  stats:              Map[String, String]
)

object ZDisplayProps:

  given Decoder[ZDisplayProps] = Decoder.instance { cu =>
    for a <- cu.downField("balanceDisplayMode").as[String]
        b <- cu.downField("images").as[Seq[Uri]]
        c <- cu.downField("label").as[Option[String]]
        d <- cu.downField("secondaryLabel").as[Option[Json]].flatMap(parseTypedValue)
        e <- cu.downField("tertiaryLabel").as[Option[Json]].flatMap(parseTypedValue)
        f <- cu.downField("info").as[Seq[Json]].flatMap(parseKeyValue).map(_.toMap)
        g <- cu.downField("stats").as[Seq[Json]].flatMap(parseKeyValue).map(_.toMap)
    yield ZDisplayProps(a, b, c, d, e, f, g)
  }

  private def parseTypedValue(maybeJson: Option[Json]): Either[DecodingFailure, Option[String]] =
    maybeJson.traverse { json =>
      val a1 = json.hcursor.downField("value").as[String]
      val a2 = json.hcursor.downField("value").as[BigDecimal]
      a1.orElse(a2.map(_.toString))
    }

  private def parseKeyValue(xs: Seq[Json]) = xs.traverse { json =>
    val lbl = json.hcursor.downField("label").downField("value").as[String]
    val v1  = json.hcursor.downField("value").downField("value").as[String]
    val v2  = json.hcursor.downField("value").downField("value").as[BigDecimal]

    lbl.flatMap(l => v1.orElse(v2.map(_.toString)).map(l -> _))
  }
