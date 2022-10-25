package com.bjoru.cryptosis.exchanges

import cats.effect.IO
import cats.syntax.traverse.given

import io.circe.*
import io.circe.syntax.{*, given}

import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.client.*
import org.http4s.client.dsl.io.*
import org.http4s.circe.*
import org.http4s.circe.CirceEntityDecoder.given
import org.http4s.client.oauth1.HmacSha256

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*
import com.bjoru.cryptosis.syntax.circe.*

import java.time.Instant

// TODO move to exchanges package and use separate config
// for this. i.e. exchanges.yaml
class BinanceUS(ep: Endpoint) extends ExchangeApi:

  given Decoder[Token] = Decoder.instance { hc =>
    for sym <- (hc <\> "coin").as[Symbol]
        fre <- (hc <\> "free").as[String]
        loc <- (hc <\> "locked").as[String]
        nlt <- (hc <\> "networkList").as[Seq[Json]]
        chs <- nlt.traverse(j => (j <\> "network").as[String])
        chx  = extractNetwork(sym, chs)
        bal  = Balance.fromBigDecimal(BigDecimal(fre) + BigDecimal(loc))
    yield Token.fromExchange(sym, chx, bal) // TODO consider special token lookup for exchanges
  }

  def signature(timestamp: Long) =
    for a <- IO.fromEither(ep.secret.fold(Left(Exception("Missing API secret!")))(Right(_)))
        b <- HmacSha256.generate[IO](s"timestamp=$timestamp", a)
    yield b

  def mkBalanceUri(ts: Long): IO[Uri] =
    for a <- IO.pure(ep.uri / "sapi" / "v1" / "capital" / "config" / "getall" )
        b <- IO.pure(a +? ("timestamp", ts.toString))
        c <- signature(ts).map(v => b +? ("signature", v))
    yield c

  def mkStakingUri(ts: Long): IO[Uri] =
    for a <- IO.pure(ep.uri / "sapi" / "v1" / "staking" / "stakingBalance")
        b <- IO.pure(a +? ("timestamp", ts.toString))
        c <- signature(ts).map(v => b +? ("signature", v))
    yield c

  def mkGet(uri: Uri) = GET(uri).withHeaders(Header("X-MBX-APIKEY", ep.apiKey))

  def sync(using client: Client[IO]): IO[Exchange] = ???

  def balance(using client: Client[IO]): IO[Seq[Token]] =
    for ts  <- IO.pure(Instant.now.toEpochMilli)
        url <- mkBalanceUri(ts)
        tok <- client.expect[Seq[Token]](url)
        res <- processTokens(tok)
    yield res

  def staking(using client: Client[IO]): IO[Seq[Defi]] =
    for ts  <- IO.pure(Instant.now.toEpochMilli)
        url <- mkStakingUri(ts)
        res <- client.expect[Json](url)
        jsn <- (res <\> "data").asIO[Seq[Json]]
        tok <- jsn.traverse(balanceTuples)
    yield ???

  private def processTokens(tokens: Seq[Token]): IO[Seq[Token]] = ???

  private def balanceTuples(json: Json): IO[(Symbol, Balance)] =
    for sym <- (json <\> "asset").asIO[Symbol]
        bal <- (json <\> "stakingAmount").asIO[Balance]
    yield sym -> bal

  private def extractNetwork(symbol: Symbol, networks: Seq[String]): Chain =
    val chains = networks.map(_.toLowerCase).map {
      case "bnb" => Chain.Binance
      case "bsc" => Chain.Binance
      case other => throw Exception(s"Missing network/chain mapping for '$other'")
    }

    // for now, grab head
    chains.distinct.head
