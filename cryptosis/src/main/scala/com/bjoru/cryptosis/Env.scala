package com.bjoru.cryptosis

import cats.effect.IO

import com.bjoru.cryptosis.types.*

class Env(registry: Map[Id, Token], priceRegistry: Map[Id, Price]):

  def price(token: Token): IO[(Env, Price)] = ???

  def prices(tokens: Seq[Token]): IO[(Env, Map[Id, Price])] = ???

  def setPrice(token: Token, price: Price): Env = 
    if registry.contains(token.id)
      then Env(registry, priceRegistry.updated(token.id, price))
      else Env(registry.updated(token.id, token), priceRegistry.updated(token.id, price))

  def setPrices(prices: Seq[(Token, Price)]): Env =
    prices.foldLeft(this) { case (acc, (t, p)) => acc.setPrice(t, p) }
