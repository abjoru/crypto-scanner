package com.bjoru.cscanner.types

object Request:

  final case class EthBalance(wallet: Wallet)

  final case class TokenBalances(wallet: Wallet, tokens: Set[Token])
