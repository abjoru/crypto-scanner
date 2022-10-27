package com.bjoru.cryptosis

import cats.effect.IO

import org.http4s.client.Client

import io.circe.*
import io.circe.syntax.*

import com.bjoru.cryptosis.types.*

final case class SyncData(
  walletAddress: Address,
  responseData:  Seq[Json]
)

object SyncData:

  given Encoder[SyncData] = Encoder.instance { data =>
    Json.obj("wallet" -> data.walletAddress.asJson, "data" -> data.responseData.asJson)
  }

  given Decoder[SyncData] = Decoder.instance { hc =>
    for wallet <- hc.downField("wallet").as[Address]
        data   <- hc.downField("data").as[Seq[Json]]
    yield SyncData(wallet, data)
  }

trait SyncResponse:

  val data: Seq[SyncData]

  /** Synchronize wallets with this response.
    * This function will convert response data to structured
    * token/defi types and register them with the environment
    * before updating the wallets.
    *
    * @param wallets list of wallets to sync.
    * @return StateT of env and updated wallets
    */
  def syncWallets(wallets: Seq[Wallet])(env: Env)(using Client[IO]): IO[(Env, Seq[Wallet])]

  /** Makes a copy of this response with the extra data added.
    *
    * @param extras data to add
    * @return new response
    */
  def withData(extras: Seq[SyncData]): SyncResponse
