package com.bjoru.cryptosis.contracts

import cats.effect.IO

import com.bjoru.cryptosis.types.*

trait DefiSync:

  def sync(wallets: Seq[Wallet])(using Web3Env): IO[Seq[Wallet]]
