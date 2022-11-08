package com.bjoru.cryptosis

import cats.effect.IO

import org.http4s.client.Client

import com.bjoru.cryptosis.types.*

trait ExchangeApi(val name: ExchangeName):

  def sync(using Client[IO]): SIO[Exchange]
