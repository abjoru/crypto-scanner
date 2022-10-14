package com.bjoru.cscanner.sol

import cats.syntax.traverse.given
import cats.effect.IO

import org.http4s.ember.client.*

import com.bjoru.cscanner.{*, given}
import com.bjoru.cscanner.types.*

import java.nio.file.Path

class SolanaApi(cfgDir: Path) extends ChainApi(Chain.Solana):

  val clientR = EmberClientBuilder.default[IO].build

  def walletBalances(wallets: Set[Wallet]): IO[Seq[TokenBalance]] =
    val xs = wallets.toList.traverse(w => clientR.use(Solscan.tokenBalances(w)))
    xs.map(_.flatten.groupBy(_.token.symbol).map(_._2.reduce(_ + _)).toSeq)

  def stakingBalances(wallets: Set[Wallet]): IO[Seq[TokenBalance]] =
    wallets.toList.traverse(w => clientR.use(Solscan.stakingBalance(w))).map(_.flatten)

  def lpBalances(wallets: Set[Wallet]): IO[Seq[LPTokenBalance]] = ???
