package com.bjoru.cscanner.eth

import cats.syntax.traverse.given
import cats.effect.IO

import org.http4s.ember.client.*

import com.bjoru.cscanner.{*, given}
import com.bjoru.cscanner.types.*
import com.bjoru.cscanner.config.*

import java.nio.file.Path

class EthereumApi(cfgDir: Path) extends ChainApi(Chain.Ethereum):

  val clientR = EmberClientBuilder.default[IO].build

  def walletBalances(wallets: Set[Wallet]): IO[Seq[TokenBalance]] = 
    val xs = wallets.toList.traverse(w => clientR.use(EthplorerProvider.tokenBalances(w)))
    xs.map(_.flatten.groupBy(_.token.symbol).map(_._2.reduce(_ + _)).toSeq)

  def stakingBalances(wallets: Set[Wallet]): IO[Seq[TokenBalance]] = IO.pure(Seq.empty)

  def lpBalances(wallets: Set[Wallet]): IO[Seq[LPTokenBalance]] = IO.pure(Seq.empty)
