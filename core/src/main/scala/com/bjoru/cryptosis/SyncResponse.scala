package com.bjoru.cryptosis

import cats.effect.IO

import io.circe.*

import com.bjoru.cryptosis.types.*

final case class SyncData(
  walletAddress: Address,
  responseData:  Seq[Json]
)

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
  def syncWallets(wallets: Seq[Wallet]): SIO[Seq[Wallet]]
