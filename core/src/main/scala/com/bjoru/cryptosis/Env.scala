package com.bjoru.cryptosis

import cats.effect.IO
import cats.syntax.traverse.*

import org.http4s.client.Client

import com.bjoru.cryptosis.types.*

object Env:

  def priceOf(token: Token): SIO[Price] = 
    SIO.inspect(_.priceOf(token))

  def valueOf(token: Token): SIO[Price] =
    SIO.inspect(_.valueOf(token))

  def valueOf(app: Defi): SIO[Price] = 
    SIO.inspect(_.valueOf(app))

  def valueOf(wallet: Wallet): SIO[Price] =
    SIO.inspect(_.valueOf(wallet))

  def valueOf(exchange: Exchange): SIO[Price] =
    SIO.inspect(_.valueOf(exchange))

  def bluechip(chain: Chain): SIO[Token] = 
    SIO(s => IO.pure(s.bluechip(chain)))

  def resolve(token: Token): SIO[Token] =
    SIO(s => IO.pure(s.resolve(token)))

  def resolveAll(tokens: Seq[Token]): SIO[Seq[Token]] =
    SIO(s => IO.pure(s.resolveAll(tokens)))

  def syncPrices(using Client[IO]): SIO[Unit] = 
    SIO.modifyF(_.syncPrices)

  def saveAll: SIO[Unit] = SIO.inspectF(_.saveAll)
