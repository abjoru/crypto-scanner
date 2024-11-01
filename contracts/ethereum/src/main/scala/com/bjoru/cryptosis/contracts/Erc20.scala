package com.bjoru.cryptosis.contracts

import cats.effect.IO
import cats.syntax.show.given

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*

import org.web3j.protocol.core.RemoteFunctionCall
import com.bjoru.cryptosis.contracts.gen.ERC20

object Erc20 extends ContractApi[ERC20]:

  protected def build(contract: Address, env: Web3Env): ERC20 = 
    ERC20.load(contract.toString, env.web3, env.txManager, env.gasProvider)

  def syncBalance(token: Token)(using Web3Env): IO[Token] =
    balanceOf(token).map(token.withBalance)

  def balanceOf(token: Token)(using Web3Env): IO[Balance] = token.contract match
    case Some(a) =>
      for b <- call(a)(_.balanceOf(a))
          d <- call(a)(_.decimals())
          r <- IO.fromTry(Balance.convert(d.intValue, b))
      yield r
    case None =>
      IO.raiseError(Exception(s"No contract address for token ${token.symbol.show}"))

  def decimalsOf(contract: Address)(using Web3Env): IO[Int] =
    call(contract)(_.decimals()).map(_.intValue)

  def nameOf(contract: Address)(using Web3Env): IO[String] =
    call(contract)(_.name())

  def symbolOf(contract: Address)(using Web3Env): IO[Symbol] =
    call(contract)(_.symbol()).map(Symbol(_))

  def totalSupplyOf(contract: Address)(using Web3Env): IO[BigInt] =
    call(contract)(_.totalSupply()).map(BigInt(_))
