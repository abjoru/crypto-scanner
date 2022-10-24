package com.bjoru.cryptosis.providers.decoders

import cats.effect.IO
import cats.syntax.show.given
import cats.syntax.traverse.given

import io.circe.*
import io.circe.parser.parse

import org.http4s.ember.client.*
import org.http4s.implicits.uri

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*
import com.bjoru.cryptosis.providers.Zapper
import com.bjoru.cryptosis.syntax.circe.*

import scala.io.Source

import munit.CatsEffectSuite

class ZapperDecoderTest extends CatsEffectSuite:

  val path = getXdgDirectory(Xdg.Cache) </> "cryptosis"

  val clientR = EmberClientBuilder.default[IO].build

  def mkWallet(addr: Address): Wallet =
    Wallet("testWallet", Chain.Ethereum, addr, Map.empty[Id, Token | Defi])

  def loadSource(name: String) = 
    Source.fromResource(s"com/bjoru/cryptos/api/$name", getClass.getClassLoader)

  //test("Decode TraderJoe") {
    //for a <- IO.fromEither(parse(loadSource("trader-joe-bal.json").getLines.mkString))
        //e <- clientR.use(Env.loadEnv(path))
        //r <- ZapperDecoder.decodeApps(a, e)
        //_ <- IO(r._2.foreach(v => println(v.show)))
    //yield assert(true)
  //}

  test("Decode all Zapper Apps") {
    val src = loadSource("resp-new-bal.json")
    val lns = src.getLines.filter(Zapper.LineFilter).toList.map(_.dropWhile(_ != '{'))

    for j  <- IO.fromEither(lns.traverse(parse(_)))
        xa <- appGroups(j)(_.filter(_._1._1 == "tokens"))
        xb <- appGroups(j)(_.filterNot(_._1._1 == "tokens"))
        e  <- clientR.use(Env.loadEnv(path))
        r1 <- ZapperDecoder.decodeTokenApps(e, xa)
        r2 <- ZapperDecoder.decodeDefiApps(r1._1, xb)
        wr  = Wallet.mergeWallets(r1._2, r2._2)
        _  <- IO(wr.foreach(_.balances.foreach(printItems)))
    yield assert(true)
  }

  def printItems(v: (Id, Token | Defi)): Unit = v match
    case (_, t: Token) => println(t.show)
    case (_, d: Defi) => println(d.show)

  def appGroups(data: Seq[Json])(
    f: Map[(String, Chain, Address), Json] => Map[(String, Chain, Address), Json]
  ): IO[Map[Wallet, Seq[Json]]] =
    val idexes = data.traverse { json =>
      for a <- (json <\> "appId").asIO[String]
          b <- (json <\> "network").asIO[Chain]
          c <- (json <\> "addresses").asIO[Seq[Address]].map(_.head)
      yield (a, b, c) -> json
    }

    val indexes = idexes.map(_.toMap).map(f)

    indexes.map(_.foldLeft(Map.empty[Wallet, Seq[Json]]) { 
      case (wx, ((_, c, a), json)) => 
        wx.updatedWith(mkWallet(a)) {
          case Some(xs) => Some(xs :+ json)
          case None     => Some(Seq(json))
        }
    })
