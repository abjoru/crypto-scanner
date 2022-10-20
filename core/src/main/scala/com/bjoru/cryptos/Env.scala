package com.bjoru.cryptos

import cats.data.StateT
import cats.effect.IO

import com.bjoru.cryptos.types.*

import java.nio.file.Path

class Env private (tokens: Map[Token, Usd], saveOnExit: Boolean = false):

  // fetch price if we don't have it?
  def price(t: Token): StateT[IO, Env, Usd] = ???

  def prices(tx: Token*): StateT[IO, Env, (Token, Usd)] = ???

  def setPrice(t: Token, p: Usd): Env =
    Env(tokens.updated(t, p))

  def setPrices(prices: Seq[(Token, Usd)]): Env =
    val result = prices.foldLeft(tokens) {
      case (acc, (t, p)) => acc.updated(t, p)
    }

    Env(result)



object Env:

  // load from disk or query coingecko if not present
  def load(file: Path): IO[Env] = ???

  def save(file: Path): IO[Unit] = ???
