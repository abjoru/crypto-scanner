package com.bjoru.cscanner.doge

import cats.syntax.traverse.given
import cats.effect.IO

import org.http4s.ember.client.*

import com.bjoru.cscanner.{*, given}
import com.bjoru.cscanner.types.*

import java.nio.file.Path

class DogecoinApi(cfgDir: Path) extends ChainApi(Chain.Dogecoin):

  val clientR = EmberClientBuilder.default[IO].build

  def balances(wallets: Set[Wallet]): IO[Seq[(Wallet, Seq[TokenBalance])]] =
    wallets.toList.traverse { wallet =>
      clientR.use(BlockCypherProvider.tokenBalance(wallet)).map(wallet -> Seq(_))
    }

  def lpBalances(wallets: Set[Wallet]): IO[Seq[LPTokenBalance]] = ???
