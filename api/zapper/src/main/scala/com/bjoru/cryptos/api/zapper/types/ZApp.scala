package com.bjoru.cryptos.api.zapper.types

import io.circe.{Decoder, Json}
import io.circe.generic.semiauto.*

import com.bjoru.cryptos.types.*

final case class ZApp(
  appId: String,
  network: String,
  addresses: Seq[Address],
  balance: ZBalance,
  totals: Seq[ZTotal],
  errors: Json,
  app: ZInfo
)

object ZApp:

  given Decoder[ZApp] = deriveDecoder[ZApp]

final case class ZInfo(
  appId: String,
  network: String,
  data: Seq[ZDescriptor],
  total: Usd
)

object ZInfo:

  given Decoder[ZInfo] = Decoder.instance { c =>
    for aid  <- c.downField("appId").as[String]
        netw <- c.downField("network").as[String]
        data <- c.downField("data").as[Seq[ZDescriptor]]
        totl <- c.downField("meta").downField("total").as[Usd]
    yield ZInfo(aid, netw, data, totl)
  }

  
