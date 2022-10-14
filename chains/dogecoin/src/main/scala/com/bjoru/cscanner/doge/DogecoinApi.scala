package com.bjoru.cscanner.doge

import cats.syntax.traverse.given
import cats.effect.IO

import org.http4s.ember.client.*

import com.bjoru.cscanner.{*, given}
import com.bjoru.cscanner.types.*

import java.nio.file.Path

class DogecoinApi(cfgDir: Path) extends ChainApi(Chain.Dogecoin):

  val clientR = EmberClientBuilder.default[IO].build

  def walletBalances(wallets: Set[Wallet]): IO[Seq[TokenBalance]] =
    wallets.toList.traverse(w => clientR.use(BlockCypherProvider.tokenBalance(w)))

  def stakingBalances(wallets: Set[Wallet]): IO[Seq[TokenBalance]] = IO.pure(Seq.empty)

  def lpBalances(wallets: Set[Wallet]): IO[Seq[LPTokenBalance]] = IO.pure(Seq.empty)
