package com.bjoru.cryptosis

import cats.effect.IO

import org.http4s.client.Client

import com.bjoru.cryptosis.types.*

trait PricingApi:

  def priceOf(token: Token): Price

  def valueOf(token: Token): Price

  def valueOf(app: Defi): Price

  def valueOf(wallet: Wallet): Price

  def valueOf(exchange: Exchange): Price

  def register(tokens: Token*): PricingApi

  def registerPrices(tokens: (Token, Price)*): PricingApi

  def syncPrices(using Client[IO]): IO[PricingApi]
