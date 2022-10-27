package com.bjoru.cryptosis.oracles

import cats.effect.IO

import org.http4s.Uri
import org.http4s.client.Client

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*

import scala.concurrent.duration.*

class CoingeckoTokens extends TokenApi:

  val geckoUri: Uri = ???

  val CACHE_FILE  = cryptosisDirectory(Xdg.Config) </> "tokens.json"
  val MISSED_FILE = cryptosisDirectory(Xdg.Config) </> "unknown-tokens.json"
  val GECKO_FILE  = cryptosisDirectory(Xdg.Cache) </> "coingecko-tokens.json"

  def resolve(tokens: Token*): IO[Seq[Token]] = ???

  def lookup(id: Id): IO[Option[Token]] = ???

  def cache: IO[Map[Id, Token]] =
    loadJson[Map[Id, Token]](CACHE_FILE).handleError(_ => Map.empty)

  def missed: IO[Map[Id, Token]] =
    loadJson[Map[Id, Token]](MISSED_FILE).handleError(_ => Map.empty)

  def geckos(using Client[IO]): IO[Map[Id, Token]] =
    if GECKO_FILE.expired(10.days)
      then fetchGeckos
      else loadJson[Map[Id, Token]](GECKO_FILE)

  private def fetchGeckos(using Client[IO]): IO[Map[Id, Token]] = ???
