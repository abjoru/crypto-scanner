package com.bjoru.cryptos.types

enum Balance2:
  case TokenBalance(token: Token, balance: BigDecimal)
  case DAppBalance(dapp: DApp, balance: BigDecimal)

final case class Wallet2(
  name: String,
  chain: Chain,
  address: Address,
  balances: Map[Id, Balance2]
)

object Wallet2:

  extension (w: Wallet2)

    def valueUsd: Usd = ???
