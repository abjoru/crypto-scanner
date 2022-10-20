package com.bjoru.cryptos.api.types

import cats.effect.IO
import cats.implicits.given

import io.circe.*
import io.circe.parser.parse

import com.bjoru.cryptos.api.ZapperApi

import scala.io.Source

import munit.CatsEffectSuite

class ZAppTest extends CatsEffectSuite:

  test("Parse TraderJoe app json") {
    val src = Source.fromResource("com/bjoru/cryptos/api/trader-joe-bal.json", getClass.getClassLoader)
    
    for a <- IO.fromEither(parse(src.getLines.mkString))
        r <- IO.fromEither(a.as[ZApp])
        //_ <- IO(r.balance.claimable.values.map(_.show).foreach(println))
        //_ <- IO(r.app.data.map(_.show).foreach(println))
        //_ <- IO(println(r))
    yield assert(true)
  }

  test("Parse all apps json") {
    val src = Source.fromResource("com/bjoru/cryptos/api/resp_zapper_bal.json", getClass.getClassLoader)
    val lns = src.getLines.filter(ZapperApi.LineFilter).toList.map(_.dropWhile(_ != '{'))

    for r <- IO.fromEither(lns.traverse(parse(_).flatMap(_.as[ZApp])))
        _ <- IO(println(r.drop(102).head))
        //_ <- IO(println(r.app.data.map(_.key).foreach(println)))
    yield assert(true)
  }
