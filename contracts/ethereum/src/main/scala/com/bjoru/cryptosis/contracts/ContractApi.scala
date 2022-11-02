package com.bjoru.cryptosis.contracts

import cats.effect.IO

import com.bjoru.cryptosis.types.*

import org.web3j.protocol.core.RemoteFunctionCall

trait ContractApi[T]:

  protected def build(contract: Address, env: Web3Env): T

  def call[U](contract: Address)(f: T => RemoteFunctionCall[U])(using env: Web3Env): IO[U] =
    IO(f(build(contract, env)).send)
