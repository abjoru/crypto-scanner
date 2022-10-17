package com.bjoru.cryptos.types

import cats.Show

import pureconfig.*
import pureconfig.generic.derivation.default.*

import com.bjoru.cryptos.utils.*

import scala.util.Try

final case class Token(
  name:     String,
  symbol:   Symbol,
  decimals: Int,
  contract: Option[Address],
  priceUsd: Option[Double],
  balance:  Option[BigDecimal]
) derives ConfigReader

object Token:

  // Bluechips
  val Btc  = Token("Bitcoin", Symbol.Btc, 8)
  val Eth  = Token("Ethereum", Symbol.Eth, 0)
  val Sol  = Token("Solana", Symbol.Sol, 8)
  val Doge = Token("Dogecoin", Symbol.Doge, 8)

  given Show[Token] = Show.show { t =>
    s"${t.name} ${t.balance.getOrElse(BigDecimal(0.0))} ${t.symbol} = $$${t.priceUsd.getOrElse(0.0)}"
  }

  extension (t: Token)

    // FIXME must be tokens of same kind!
    def +(other: Token): Token = 
      t.copy(balance = t.balance.map(_ + other.balance.getOrElse(BigDecimal(0.0))))

    def withContract(contract: Address): Token =
      t.copy(contract = Some(contract))

    def withPrice(price: Double): Token =
      t.copy(priceUsd = Some(price))

    def withBalance(balance: BigDecimal): Token =
      t.copy(balance = Some(balance))

    def withRawBalance(balance: String): Try[Token] =
      decodeQuantity(balance).map(n => t.withBalance(BigDecimal(n) / math.pow(10, t.decimals)))

    def withRawBalance(balance: BigInt): Token =
      t.withBalance(BigDecimal(balance) / math.pow(10, t.decimals))

    def valueUsd: Double = (t.balance, t.priceUsd) match
      case (Some(b), Some(p)) => (b * BigDecimal(p)).toDouble
      case _                  => 0.0

  def apply(name: String, symbol: Symbol, decimals: Int): Token =
    new Token(name, symbol, decimals, None, None, None)

  def TokenId(t: Token) = (t.symbol, t.contract)

  def forChain(chain: Chain): Option[Token] = chain match
    case Chain.Bitcoin  => Some(Btc)
    case Chain.Ethereum => Some(Eth)
    case Chain.Solana   => Some(Sol)
    case Chain.Dogecoin => Some(Doge)
    case _              => None


  def consolidate(tokens: Seq[Token]): Seq[Token] = 
    val first  = Map.empty[Address, Seq[Token]]
    val second = Map.empty[Symbol, Seq[Token]]

    val (byContract, bySymbol) = tokens.foldLeft(first -> second) {
      case ((bc, bs), t@Token(_, _, _, Some(a), _, _)) if bc.contains(a) => 
        (bc.updatedWith(a)(_.map(_ :+ t)), bs)
      case ((bc, bs), t@Token(_, _, _, Some(a), _, _)) => 
        (bc + (a -> Seq(t)), bs)
      case ((bc, bs), t) if bs.contains(t.symbol) => 
        (bc, bs.updatedWith(t.symbol)(_.map(_ :+ t)))
      case ((bc, bs), t) => 
        (bc, bs + (t.symbol -> Seq(t)))
    }

    val r1 = byContract.mapValues(_.reduce(_ + _)).values
    val r2 = bySymbol.mapValues(_.reduce(_ + _)).values

    r1.toSeq ++ r2.toSeq

  def valueUsd(tokens: Seq[Token]): Double = 
    consolidate(tokens).foldLeft(0.0) {
      case (acc, t) => acc + t.valueUsd
    }
