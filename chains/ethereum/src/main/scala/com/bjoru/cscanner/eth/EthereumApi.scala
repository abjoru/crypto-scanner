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

  def balances(wallets: Set[Wallet]): IO[Seq[(Wallet, Seq[TokenBalance])]] = 
    wallets.toList.traverse { wallet =>
      clientR.use(EthplorerProvider.tokenBalances(wallet)(using _).map(wallet -> _))
    }

  def lpBalances(wallets: Set[Wallet]): IO[Seq[LPTokenBalance]] = ???