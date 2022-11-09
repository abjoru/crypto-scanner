package com.bjoru.cryptosis

import cats.effect.IO

import org.http4s.ember.client.EmberClientBuilder

import com.bjoru.cryptosis.types.*
import com.bjoru.cryptosis.oracles.CoingeckoOracle

import munit.CatsEffectSuite
import java.nio.file.Paths

class StateTest extends CatsEffectSuite:

  val tmpDir: FilePath = Paths.get(System.getProperty("java.io.tmpdir"))

  val tFile = tmpDir </> "tokens.json"
  val uFile = tmpDir </> "unknowns.json"
  val gFile = tmpDir </> "geckos.json"

  val oracle = new CoingeckoOracle

  val clientR = EmberClientBuilder.default[IO].build

  test("Resolve known token") {
    for s <- clientR.use(State(tFile, uFile, gFile, oracle)(using _))
        t  = Token.simple("MonkeyBucks", Symbol("mbs"), Chain.Solana)
        r <- State.resolve(t).runA(s)
        _ <- putStrLn(r._2.toString)
        _ <- IO(s.tokens.foreach(println))
        _ <- IO(s.oracleTokens.filter(_._2.chain == Chain.Solana).foreach(println))
    yield assert(true)
  }
