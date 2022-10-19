package com.bjoru.cryptos.api.types

import io.circe.{Decoder, Json}

import com.bjoru.cryptos.types.*

final case class ZBalance(
  deposits:  Json,
  debt:      Json,
  vesting:   Json,
  wallet:    Map[String, ZDescriptor],
  claimable: Map[String, ZDescriptor],
  locked:    Json,
  nft:       Json,
  displayProps: Option[Json],
  meta: Option[Json]
)

object ZBalance:

  given Decoder[ZBalance] = Decoder.instance { c =>
    for dep  <- c.downField("deposits").as[Json]
        debt <- c.downField("debt").as[Json]
        vest <- c.downField("vesting").as[Json]
        wall <- c.downField("wallet").as[Map[String, ZDescriptor]]
        clm  <- c.downField("claimable").as[Map[String, ZDescriptor]]
        lckd <- c.downField("locked").as[Json]
        nft  <- c.downField("nft").as[Json]
        dprp <- c.downField("displayProps").as[Option[Json]]
        mta  <- c.downField("meta").as[Option[Json]]
    yield ZBalance(dep, debt, vest, wall, clm, lckd, nft, dprp, mta)
  }
