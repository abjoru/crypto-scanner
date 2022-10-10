package com.bjoru.cscanner

import cats.effect.IO

import com.bjoru.cscanner.types.*

trait ChainApi:

  def balances(wallets: Address*): IO[Seq[TokenBalance]]

  def lpBalances(wallets: Address*): IO[Seq[LPTokenBalance]]
