package com.bjoru.cscanner.egld

import cats.syntax.traverse.given
import cats.effect.IO

import org.http4s.ember.client.*

import com.bjoru.cscanner.{*, given}
import com.bjoru.cscanner.types.*
import com.bjoru.cscanner.config.*

import java.nio.file.Path

class ElrondApi(cfgDir: Path) extends ChainApi(Chain.Elrond):

  import Balance.*

  val clientR = EmberClientBuilder.default[IO].build

  def walletBalances(wallets: Set[Wallet]): IO[Seq[TokenBalance]] =
    val xs = wallets.toList.traverse(w => clientR.use(ElrondProvider.getTokens(w)))
    xs.map(v => Balance.flattenTokens(v.flatten))

  def stakingBalances(wallets: Set[Wallet]): IO[Seq[StakingBalance]] = IO.pure(Seq.empty)

  def farmBalances(wallets: Set[Wallet]): IO[Seq[FarmBalance]] = IO.pure(Seq.empty)
