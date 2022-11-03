package com.bjoru.cryptosis

import cats.effect.IO

import org.http4s.client.Client

import com.bjoru.cryptosis.types.*

trait Oracle:

  /** Fetch tokens from upstream oracle.
    *
    * @param file token cache for oracle
    * @return map of id to token
    */
  def fetchTokens(file: FilePath)(using Client[IO]): IO[Map[Id, Token]]

  /** Fetch token prices from upstream oracle.
    *
    * @param tokens target tokens
    * @return map of token id to price
    */
  def fetchPrices(tokens: Seq[Token])(using Client[IO]): IO[Map[Id, Price]]
