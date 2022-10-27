package com.bjoru.cryptosis

import cats.effect.IO

import com.bjoru.cryptosis.types.*

trait TokenApi:

  /** Resolves a given token against the oracle's token
    * list such that pricing can be achieved. This function
    * will move any resolved token into the known token 
    * cache, and by dual, unresolved tokens to the unknown
    * token cache. These caches are backed by files which
    * can easily be edited manually if needed.
    *
    * The TokenApi should always load caches during bootstrap
    * procedures.
    */
  def resolve(tokens: Token*): IO[Seq[Token]]

  def lookup(id: Id): IO[Option[Token]]

  def lookup(symbol: Symbol, chain: Chain): IO[Option[Token]] =
    lookup(Token.mkId(symbol, chain))
