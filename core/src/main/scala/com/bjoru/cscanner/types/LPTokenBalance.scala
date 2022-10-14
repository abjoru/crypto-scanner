package com.bjoru.cscanner.types

final case class LPTokenBalance(
  contract:       Address,
  tokenBalances:  Seq[TokenBalance],
  rewardBalances: Seq[TokenBalance]
)

object LPTokenBalance:

  extension (lp: LPTokenBalance)
    def toTokenBalances: Seq[TokenBalance] =
      val tm = lp.tokenBalances.foldLeft(Map.empty[Symbol, TokenBalance]) {
        case (acc, tb) if acc.contains(tb.token.symbol) =>
          acc.updatedWith(tb.token.symbol)(_.map(_ + tb))
        case (acc, tb) =>
          acc + (tb.token.symbol -> tb)
      }

      val res = lp.rewardBalances.foldLeft(tm) {
        case (acc, tb) if acc.contains(tb.token.symbol) =>
          acc.updatedWith(tb.token.symbol)(_.map(_ + tb))
        case (acc, tb) =>
          acc + (tb.token.symbol -> tb)
      }

      res.values.toSeq
