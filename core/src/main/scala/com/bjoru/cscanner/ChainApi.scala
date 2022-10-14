package com.bjoru.cscanner

import cats.Show
import cats.effect.IO

import pureconfig.ConfigReader
import pureconfig.error.ExceptionThrown

import com.bjoru.cscanner.api.Coingecko
import com.bjoru.cscanner.types.*

import scala.util.{Try, Success, Failure}

import java.nio.file.Path

enum Chain:
  case Bitcoin
  case Ethereum
  case Solana
  case Binance
  case Avalanche
  case Fantom
  case Polygon
  case Elrond
  case Harmony
  case Dogecoin

object Chain:

  given Show[Chain] = Show.fromToString[Chain]

  given ConfigReader[Chain] = ConfigReader.fromString { str =>
    Try(Chain.valueOf(str)) match
      case Success(ch) => Right(ch)
      case Failure(er) => Left(ExceptionThrown(er))
  }

  extension (c: Chain)
    def symbol: Symbol = c match
      case Chain.Bitcoin => Symbol("btc")
      case Chain.Ethereum => Symbol("eth")
      case Chain.Solana => Symbol("sol")
      case Chain.Binance => Symbol("bnb")
      case Chain.Avalanche => Symbol("avax")
      case Chain.Fantom => Symbol("ftm")
      case Chain.Polygon => Symbol("matic")
      case Chain.Elrond => Symbol("egld")
      case Chain.Harmony => Symbol("one")
      case Chain.Dogecoin => Symbol("doge")

  def fromString(str: String): Option[Chain] =
    Try(Chain.valueOf(str.capitalize)).toOption

trait ChainApi(val chain: Chain):
  import Balance.*

  def totals(cfgDir: Path, wallets: Set[Wallet]): IO[(Double, Seq[TokenBalance])] = 
    for a <- balances(wallets)
        b <- priceUsd(cfgDir, a)
        _ <- IO(b.foreach(println))
        r  = b.foldLeft(0.0)(_ + _.valueUsd.toDouble)
    yield r -> b

  def balances(wallets: Set[Wallet]): IO[Seq[TokenBalance]] =
    for wb <- walletBalances(wallets)
        sb <- stakingBalances(wallets)
        lp <- farmBalances(wallets)
    yield Balance.flattenTokens(wb ++ sb.map(_.toTokenBalance) ++ lp.foldLeft(Seq.empty[TokenBalance])(_ ++ _.toTokenBalances))

  def walletBalances(wallets: Set[Wallet]): IO[Seq[TokenBalance]]

  def stakingBalances(wallets: Set[Wallet]): IO[Seq[StakingBalance]]

  def farmBalances(wallets: Set[Wallet]): IO[Seq[FarmBalance]]

  def priceUsd(cfgDir: Path, balances: Seq[TokenBalance]): IO[Seq[TokenBalance]] =
    val (missing, ok) = balances.partition(_.isMissingPrice)

    if missing.isEmpty then IO.pure(ok) else 
      Coingecko(cfgDir).priceFor(missing).map(_ ++ ok)
