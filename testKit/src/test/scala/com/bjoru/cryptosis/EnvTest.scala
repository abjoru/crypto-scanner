package com.bjoru.cryptosis

import cats.effect.IO

import org.http4s.ember.client.*
import org.http4s.implicits.uri

import com.bjoru.cryptosis.types.*

import munit.CatsEffectSuite

class EnvTest extends CatsEffectSuite:

  val cfgDir = getXdgDirectory(Xdg.Config) </> "cryptosis"

  val clientR = EmberClientBuilder.default[IO].build

  test("load empty registry") {
    for e <- clientR.use(Env.loadRegistry(cfgDir </> "tokens.json"))
        //_ <- IO(e.registry.foreach(kv => println(kv._2.name)))
        //_ <- IO(println(e.registry.size))
        r  = e.findToken(Symbol("joe"), Chain.Unknown, None)
        _ <- IO(println(r))
    yield assert(true)
  }
