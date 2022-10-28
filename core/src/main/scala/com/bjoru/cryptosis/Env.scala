package com.bjoru.cryptosis

import cats.effect.IO

import org.http4s.client.Client

import com.bjoru.cryptosis.types.*

import scala.util.Try

trait Env:

  val priceApi: PricingApi

  /** Register items allows the system to resolve token data
    * from oracle (coingecko as of now) such that we can later
    * fetch prices for them. 
    *
    * Each token is registered with Symbol/Chain pairs as 
    * unique identifiers. Resolved tokens (tokens that were
    * matched with coingecko), will be written to persistent
    * cache for simple retrieval. This cache will also contain
    * unmatched (unresolved) tokens such that they can be manually
    * edited if needed.
    *
    * @param items tokens or defi items to register.
    * @return updated env and item list.
    */
  def register(items: Seq[Token | Defi])(using Client[IO]): IO[Result[Seq[Token | Defi]]]

  def registerToken(token: Token)(using Client[IO]): IO[Result[Token]] =
    register(Seq(token)).flatMap {
      case Result(e, Seq(t: Token)) => IO.pure(Result(e, t))
      case other                    => IO.raiseError(Exception(s"Token registration failed! Got $other"))
    }

  def registerDefi(defi: Defi)(using Client[IO]): IO[Result[Defi]] =
    register(Seq(defi)).flatMap {
      case Result(e, Seq(d: Defi)) => IO.pure(Result(e, d))
      case other                   => IO.raiseError(Exception(s"Defi registration failed! Got $other"))
    }

  def registerWithPrice(tokens: Seq[(Token, Option[Price])])(using Client[IO]): IO[Result[Seq[Token]]]

  def registerTokenPrice(token: Token, price: Option[Price])(using Client[IO]): IO[Result[Token]] = 
    registerWithPrice(Seq(token -> price)).flatMap {
      case Result(e, Seq(t)) => IO.pure(Result(e, t))
      case other             => IO.raiseError(Exception(s"Token price registration failed! Got $other"))
    }

  /** Get bluechip token (gas/network token) for a given chain.
    * Bluechip tokens should always be registered, so this 
    * function does not require an effect.
    *
    * @param chain target chain.
    * @return Token bluechip token.
    */
  def bluechip(chain: Chain)(using Client[IO]): IO[Token]

  /** Query the underlying oracle for tokens given by token
    * symbol and chain. Due to the registration facility,
    * this function does not require effects, as tokens
    * returned by providers should already be known.
    *
    * @param symbol token symbol.
    * @param chain token chain.
    * @return maybe token.
    */
  def token(symbol: Symbol, chain: Chain)(using Client[IO]): IO[Option[Token]]

  def syncPrices(using Client[IO]): IO[Env]

  /** Create a copy (mutation) of this environment. 
    *
    * @param f copy function
    * @return mutated env
    */
  def copy(f: Env => Env): Env = f(this)

  /** Create an effectful copy of this environment.
    * 
    *
    * @param f copy function.
    * @return mutated env effect
    */
  def copyM(f: Env => IO[Env]): IO[Env] = f(this)
