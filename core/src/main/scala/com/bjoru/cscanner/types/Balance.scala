package com.bjoru.cscanner.types

import cats.Show
import cats.syntax.show.given

import io.circe.*

import scala.util.Try
import scala.math.BigDecimal.RoundingMode

enum Balance:
  case TokenBalance(token: Token, balance: BigDecimal)
  case StakingBalance(token: Token, balance: BigDecimal)
  case FarmBalance(id: String, lp: Seq[TokenBalance], rewards: Seq[TokenBalance])
  case LpBalance(poolToken: TokenBalance, pairs: Seq[TokenBalance])

object Balance:

  given Show[TokenBalance] = Show.show {
    case tb@TokenBalance(t, b) =>
      s"${b.setScale(4, RoundingMode.HALF_UP)} ${t.symbol.show} = $$${tb.valueUsd.toDouble}"
  }

  extension (tb: Balance.TokenBalance)
    def +(other: Balance.TokenBalance): Balance.TokenBalance =
      Balance.TokenBalance(tb.token, tb.balance + other.balance)

    def withPrice(price: Double): Balance.TokenBalance = 
      tb.copy(token = tb.token.withPrice(price))

  extension (sb: Balance.StakingBalance)
    def +(other: Balance.StakingBalance): Balance.StakingBalance =
      Balance.StakingBalance(sb.token, sb.balance + other.balance)

    def withPrice(price: Double): Balance.StakingBalance = 
      sb.copy(token = sb.token.withPrice(price))

    def toTokenBalance: Balance.TokenBalance = 
      Balance.TokenBalance(sb.token, sb.balance)

  extension (fb: Balance.FarmBalance)
    def withPrice(f: Symbol => Double): Balance.FarmBalance =
      val newLp = fb.lp.map(v => v.withPrice(f(v.token.symbol)))
      val newRw = fb.rewards.map(v => v.withPrice(f(v.token.symbol)))
      Balance.FarmBalance(fb.id, newLp, newRw)

    def toTokenBalances: Seq[Balance.TokenBalance] = 
      flattenTokens(fb.lp ++ fb.rewards)

  extension (b: Balance)

    def valueUsd: BigDecimal = b match
      case Balance.TokenBalance(t, b) => t.priceUsd match
        case Some(p) => Try(b * BigDecimal(p)).getOrElse(BigDecimal(0.0))
        case _       => BigDecimal(0.0)
      case Balance.StakingBalance(t, b) => 
        t.priceUsd.map(p => b * BigDecimal(p)).getOrElse(BigDecimal(0.0))
      case Balance.FarmBalance(_, lp, rw) =>
        (lp ++ rw).foldLeft(BigDecimal(0.0))(_ + _.valueUsd)

    def isMissingPrice: Boolean = b match
      case Balance.TokenBalance(t, _) => t.priceUsd.isEmpty
      case Balance.StakingBalance(t, _) => t.priceUsd.isEmpty
      case Balance.FarmBalance(_, ax, bx) => (ax ++ bx).filter(_.token.priceUsd.isEmpty).nonEmpty


  def flattenTokens(tbs: Seq[Balance.TokenBalance]): Seq[Balance.TokenBalance] = 
    tbs.groupBy(_.token.symbol).map(_._2.reduce(_ + _)).toSeq

  def flattenStaking(sbs: Seq[Balance.StakingBalance]): Seq[Balance.StakingBalance] =
    sbs.groupBy(_.token.symbol).map(_._2.reduce(_ + _)).toSeq

  def flattenFarms(fbs: Seq[Balance.FarmBalance]): Seq[Balance.FarmBalance] =
    fbs.groupBy(_.id).map(_._2.reduce {
      case (a, b) =>
        val newLp = flattenTokens(a.lp ++ b.lp)
        val newRw = flattenTokens(a.rewards ++ b.rewards)
        Balance.FarmBalance(a.id, newLp, newRw)
    }).toSeq
