package com.bjoru.cscanner

import cats.Show
import cats.effect.IO

import pureconfig.ConfigReader
import pureconfig.error.ExceptionThrown

import com.bjoru.cscanner.types.*

import scala.util.{Try, Success, Failure}

enum Chain:
  case Ethereum
  case Solana
  case Binance
  case Avalanche
  case Fantom
  case Polygon
  case Elrond
  case Harmony

object Chain:

  given Show[Chain] = Show.fromToString[Chain]

  given ConfigReader[Chain] = ConfigReader.fromString { str =>
    Try(Chain.valueOf(str)) match
      case Success(ch) => Right(ch)
      case Failure(er) => Left(ExceptionThrown(er))
  }

trait ChainApi(chain: Chain):

  def balances(wallets: Address*): IO[Seq[TokenBalance]]

  def lpBalances(wallets: Address*): IO[Seq[LPTokenBalance]]
