package com.bjoru.cryptosis.pricing

import cats.effect.IO

import org.http4s.client.Client

import com.bjoru.cryptosis.types.*

trait Pricing:

  def priceOf(token: Token): Price

  def valueOf(token: Token): Price

  def valueOf(app: Defi): Price

  def valueOf(wallet: Wallet): Price

  def valueOf(exchange: Exchange): Price

  def register(tokens: Token*): Pricing

  def registerPrices(tokens: (Token, Price)*): Pricing

  def syncPrices(using Client[IO]): IO[Pricing]
