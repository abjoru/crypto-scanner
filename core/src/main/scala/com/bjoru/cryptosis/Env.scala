package com.bjoru.cryptosis

import cats.effect.IO

import com.bjoru.cryptosis.types.*

import scala.util.Try

trait Env:

  val priceApi: PricingApi

  /** Price any tokens that does not yet have price data.
    * This function will resolve all prices for registered
    * tokens and apply them to the wallets. 
    *
    * @param wallets list of wallets to update.
    * @return updated env and wallet list.
    */
  def price(wallets: Seq[Wallet]): IO[Result[Seq[Wallet]]]

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
  def register(items: (Token | Defi)*): IO[Result[Seq[Token | Defi]]]

  /** Get bluechip token (gas/network token) for a given chain.
    * Bluechip tokens should always be registered, so this 
    * function does not require an effect.
    *
    * @param chain target chain.
    * @return Token bluechip token.
    */
  def bluechip(chain: Chain): Token

  /** Query the underlying oracle for tokens given by token
    * symbol and chain. Due to the registration facility,
    * this function does not require effects, as tokens
    * returned by providers should already be known.
    *
    * @param symbol token symbol.
    * @param chain token chain.
    * @return maybe token.
    */
  def token(symbol: Symbol, chain: Chain): Option[Token]

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
