package com.bjoru.cryptos.api.zapper.types

import cats.effect.IO
import cats.implicits.given

import io.circe.parser.parse

import scala.io.Source

import munit.CatsEffectSuite

class ZAppTest extends CatsEffectSuite:

  test("Parse TraderJoe app json") {
    val src = Source.fromResource("com/bjoru/cryptos/api/trader-joe-bal.json", getClass.getClassLoader)
    
    for a <- IO.fromEither(parse(src.getLines.mkString))
        r <- IO.fromEither(a.as[ZApp])
        //_ <- IO(r.balance.claimable.values.map(_.show).foreach(println))
        //_ <- IO(r.app.data.map(_.show).foreach(println))
        _ <- IO(println(r.app.data.map(_.key).foreach(println)))
    yield assert(true)
  }
