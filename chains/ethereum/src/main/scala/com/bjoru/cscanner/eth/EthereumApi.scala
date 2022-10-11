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
      for api  <- QuickNodeProvider.loadProvider(cfgDir, Chain.Ethereum)
          ts   <- loadTokens(cfgDir </> TOKENS_FILE).map(_(chain).toSet)
          eth  <- clientR.use(api.getEthBalance(wallet)(using _))
          toks <- clientR.use(api.getTokenBalance(wallet, ts)(using _))
      yield wallet -> (eth +: toks)
    }

  def lpBalances(wallets: Set[Wallet]): IO[Seq[LPTokenBalance]] = ???
