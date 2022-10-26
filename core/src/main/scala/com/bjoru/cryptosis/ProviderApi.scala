package com.bjoru.cryptosis

import cats.effect.IO

import org.http4s.client.Client

import com.bjoru.cryptosis.types.*
import com.bjoru.cryptosis.instances.given

trait ProviderApi:

  def sync(wallets: Seq[Wallet])(using Client[IO]): IO[SyncResponse]
