package com.bjoru.cscanner.avax

import cats.syntax.traverse.given
import cats.effect.IO

import org.http4s.ember.client.*

import com.bjoru.cscanner.{*, given}
import com.bjoru.cscanner.types.*

import java.nio.file.Path

class AvalancheApi(cfgDir: Path) extends ChainApi(Chain.Avalanche):

  import Balance.*

  val clientR = EmberClientBuilder.default[IO].build

  def walletBalances(wallets: Set[Wallet]): IO[Seq[TokenBalance]] =
    val xs = wallets.toList.traverse(w => clientR.use(CovalentApi(chain).tokenBalance(w)))
    xs.map(_.flatten.groupBy(_.token.symbol).map(_._2.reduce(_ + _)).toSeq)


  def stakingBalances(wallets: Set[Wallet]): IO[Seq[StakingBalance]] = IO.pure(Seq.empty)

  def lpBalances(wallets: Set[Wallet]): IO[Seq[FarmBalance]] = IO.pure(Seq.empty)
