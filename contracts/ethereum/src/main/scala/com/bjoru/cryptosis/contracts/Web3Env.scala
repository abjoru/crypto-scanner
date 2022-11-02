package com.bjoru.cryptosis.contracts

import org.http4s.Uri

import com.bjoru.cryptosis.types.*

import org.web3j.protocol.Web3j
import org.web3j.tx.ReadonlyTransactionManager
import org.web3j.tx.gas.ContractGasProvider
import org.web3j.protocol.core.RemoteFunctionCall
import org.web3j.tx.gas.DefaultGasProvider
import org.web3j.protocol.http.HttpService

class Web3Env(val web3: Web3j, fromAddress: Address):
  val txManager   = ReadonlyTransactionManager(web3, fromAddress)
  val gasProvider = DefaultGasProvider()

object Web3Env:

  def build(fromAddress: Address, nodeUri: Uri): Web3Env =
    Web3Env(Web3j.build(new HttpService(nodeUri.toString)), fromAddress)
