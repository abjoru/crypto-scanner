package com.bjoru.cryptosis.contracts
package mc

import cats.effect.IO
import cats.syntax.show.given
import cats.syntax.traverse.given

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*

import com.bjoru.cryptosis.contracts.gen.View

object MeritCircle extends ContractApi[View] with DefiSync:

  protected def build(contract: Address, env: Web3Env): View = 
    View.load(contract.toString, env.web3, env.txManager, env.gasProvider)

  def sync(wallets: Seq[Wallet])(using Web3Env): IO[Seq[Wallet]] = wallets.traverse { 
    case wallet if wallet.chain == Chain.Ethereum => rawData(wallet).map(processData)
    case wallet => IO.pure(wallet)
  }

  def rawData(wallet: Wallet)(using Web3Env): IO[View.Data] =
    if wallet.chain == Chain.Ethereum
      then call(wallet.address)(_.fetchData(wallet.address))
      else IO.raiseError(Exception(s"MeritCircle only supports Ethereum wallets!"))

  private def processData(data: View.Data): Wallet = ???
