package com.bjoru.cscanner

import cats.Show
import cats.effect.IO

import pureconfig.ConfigReader
import pureconfig.error.ExceptionThrown

import com.bjoru.cscanner.types.*

import scala.util.{Try, Success, Failure}

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


trait ChainApi(val chain: Chain):

  def balances(wallets: Set[Wallet]): IO[Seq[(Wallet, Seq[TokenBalance])]]

  def lpBalances(wallets: Set[Wallet]): IO[Seq[LPTokenBalance]]
