package com.bjoru.cscanner.types

final case class LPTokenBalance(
  contract:       Address,
  tokenBalances:  Seq[TokenBalance],
  rewardBalances: Seq[TokenBalance]
)
