package com.bjoru.cryptosis.exchanges

import cats.effect.IO

import org.http4s.client.oauth1.HmacSha256

import com.bjoru.cryptosis.*

// TODO move to exchanges package and use separate config
// for this. i.e. exchanges.yaml
class BinanceUS(ep: Endpoint):

  def signature(timestamp: Long) =
    for a <- IO.fromEither(ep.secret.fold(Left(Exception("Missing API secret!")))(Right(_)))
        b <- HmacSha256.generate[IO](s"timestamp=$timestamp", a)
    yield b
