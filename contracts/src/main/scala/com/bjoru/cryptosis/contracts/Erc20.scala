package com.bjoru.cryptosis.contracts

import cats.effect.IO
import cats.syntax.show.given

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*
import com.bjoru.cryptosis.tokens.erc20.ERC20

import org.web3j.protocol.core.RemoteFunctionCall

object Erc20 extends ContractApi[ERC20]:

  protected def build(contract: Address, env: Web3Env): ERC20 =
    ERC20.load(contract.toString, env.web3, env.txManager, env.gasProvider)

  def balanceOf(token: Token)(using e: Web3Env): IO[Balance] = token.contract match
    case Some(a) => 
      for b <- call(a)(_.balanceOf(a))
          d <- call(a)(_.decimals())
          r <- IO.fromTry(Balance.convert(d.intValue, b))
      yield r
    case None =>
      IO.raiseError(Exception(s"No contract address for token ${token.symbol.show}"))


  def decimalsOf(contract: Address)(using Web3Env): IO[Int] =
    call(contract)(_.decimals).map(_.intValue)

  def nameOf(contract: Address)(using Web3Env): IO[String] =
    call(contract)(_.name)

  def symbolOf(contract: Address)(using Web3Env): IO[Symbol] =
    call(contract)(_.symbol).map(Symbol(_))

  def totalSupplyOf(contract: Address)(using Web3Env): IO[BigInt] =
    call(contract)(_.totalSupply).map(BigInt(_))
