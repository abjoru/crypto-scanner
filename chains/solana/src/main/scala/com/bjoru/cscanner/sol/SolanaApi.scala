package com.bjoru.cscanner.sol

import cats.syntax.traverse.given
import cats.effect.IO

import org.http4s.ember.client.*

import com.bjoru.cscanner.{*, given}
import com.bjoru.cscanner.types.*
import com.bjoru.cscanner.api.CovalentApi

import java.nio.file.Path

class SolanaApi(cfgDir: Path) extends ChainApi(Chain.Solana):

  import Balance.*

  val clientR = EmberClientBuilder.default[IO].build

  def walletBalances(wallets: Set[Wallet]): IO[Seq[TokenBalance]] =
    val xs = wallets.toList.traverse(w => clientR.use(CovalentApi(cfgDir, chain).tokenBalance(w)))
    xs.map(_.flatten.groupBy(_.token.symbol).map(_._2.reduce(_ + _)).toSeq)


  def stakingBalances(wallets: Set[Wallet]): IO[Seq[StakingBalance]] =
    wallets.toList.traverse(w => clientR.use(Solscan.stakingBalance(w))).map(_.flatten)

  def farmBalances(wallets: Set[Wallet]): IO[Seq[FarmBalance]] = IO.pure(Seq.empty)
