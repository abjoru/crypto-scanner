package com.bjoru.cscanner.egld

import cats.syntax.traverse.given
import cats.effect.IO

import org.http4s.ember.client.*

import com.bjoru.cscanner.{*, given}
import com.bjoru.cscanner.types.*
import com.bjoru.cscanner.config.*

import java.nio.file.Path

class ElrondApi(cfgDir: Path) extends ChainApi(Chain.Elrond):

  val clientR = EmberClientBuilder.default[IO].build

  def balances(wallets: Set[Wallet]): IO[Seq[(Wallet, Seq[TokenBalance])]] =
    wallets.toList.traverse { wallet =>
      for api <- ElrondProvider.loadProvider(cfgDir)
          tok <- clientR.use(api.getTokens(wallet)(using _))
      yield wallet -> tok
    }

  def lpBalances(wallets: Set[Wallet]): IO[Seq[LPTokenBalance]] = ???
