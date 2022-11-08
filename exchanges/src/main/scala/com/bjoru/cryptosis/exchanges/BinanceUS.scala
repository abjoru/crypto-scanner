package com.bjoru.cryptosis.exchanges

import cats.effect.IO
import cats.syntax.traverse.given

import io.circe.*
import DecodingFailure as DF

import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.client.*
import org.http4s.client.dsl.io.*
import org.http4s.circe.*
import org.http4s.circe.CirceEntityDecoder.given
import org.http4s.client.oauth1.HmacSha256

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*

import java.time.Instant

class BinanceUS(ep: Endpoint) extends ExchangeApi(ExchangeName.BinanceUS):

  given Decoder[ExchangeToken] = Decoder.instance { hc =>
    for sym <- hc.downField("coin").as[Symbol]
        fre <- hc.downField("free").as[String]
        loc <- hc.downField("locked").as[String]
        nlt <- hc.downField("networkList").as[Seq[Json]]
        chs <- nlt.traverse(j => j.hcursor.downField("network").as[String])
        chx <- extractNetwork(sym, chs)
        ba1 <- Balance.fromString(fre).toCirce(hc)
        ba2 <- Balance.fromString(loc).toCirce(hc)
        bal  = ba1 + ba2
    yield ExchangeToken(sym, chx, bal)
  }

  val SupportedStakingTokens = Map(
    Symbol.Dot -> "polkadot"
  )

  def sync(using Client[IO]): SIO[Exchange] = ???

  def balance(using client: Client[IO]): SIO[Seq[Token]] =
    for ts  <- SIO.pure(Instant.now.toEpochMilli)
        url <- SIO.liftF(mkBalanceUri(ts))
        get <- SIO.liftF(mkGet(url))
        tok <- SIO.liftF(client.expect[Seq[ExchangeToken]](get))
        res <- SIO(s => IO.pure(s.exchangeTokens(tok)))
    yield res

  private def signature(timestamp: Long) = 
    for a <- IO.fromOption(ep.secret)(Exception("Missing API secret for BinanceUS!"))
        b <- HmacSha256.generate[IO](s"timestamp=$timestamp", a)
    yield b

  private def mkBalanceUri(ts: Long): IO[Uri] =
    for a <- IO.pure(ep.uri / "sapi" / "v1" / "capital" / "config" / "getall")
        b <- IO.pure(a +? ("timestamp", ts.toString))
        c <- signature(ts).map(v => b +? ("signature", v))
    yield c

  private def mkStakingUri(ts: Long): IO[Uri] =
    for a <- IO.pure(ep.uri / "sapi" / "v1" / "staking" / "stakingBalance")
        b <- IO.pure(a +? ("timestamp", ts.toString))
        c <- signature(ts).map(v => b +? ("signature", v))
    yield c

  private def mkGet(uri: Uri) = ep.apiKey match
    case Some(key) => IO.pure(GET(uri).withHeaders(Header("X-MBX-APIKEY", key)))
    case None      => IO.raiseError(Exception("No ApiKey for BinanceUS!"))

  private def extractNetwork(symbol: Symbol, networks: Seq[String]): Decoder.Result[Chain] =
    val maybeChain = networks.map(_.toLowerCase).collectFirst {
      case "bnb" => Chain.Binance
      case "bsc" => Chain.Binance
    }

    maybeChain.toCirce(s"No binance symbol/chain mapping for $symbol: $networks")