package com.bjoru.cryptosis

import cats.effect.IO

import org.http4s.client.*

import com.bjoru.cryptosis.types.*

trait ExchangeApi:

  def sync(env: Env, client: Client[IO]): IO[(Env, Exchange)] = ???
