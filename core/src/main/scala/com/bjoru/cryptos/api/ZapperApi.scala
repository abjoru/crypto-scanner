package com.bjoru.cryptos.api

import cats.effect.IO
import cats.syntax.traverse.given

import io.circe.Json
import io.circe.parser.parse

import org.http4s.*
import org.http4s.headers.*
import org.http4s.dsl.io.*
import org.http4s.client.*
import org.http4s.client.dsl.io.*
import org.http4s.circe.*

import com.bjoru.cryptos.types.*
import com.bjoru.cryptos.api.types.*

import scala.io.Source
import scala.util.{Try, Success, Failure}

import java.nio.file.Path

class ZapperApi(endpoint: Endpoint) extends CryptoApi:

  import ZapperApi.*

  val supportedChains = Seq(
    Chain.Ethereum,
    Chain.Binance,
    Chain.Avalanche,
    Chain.Fantom,
    Chain.Polygon,
    Chain.Harmony
  )

  val Auth = Authorization(BasicCredentials(endpoint.apiKey, ""))

  def doSyncWallets(wallets: Seq[Wallet], client: Client[IO]): IO[Seq[Wallet]] = ???

  def balances(wallets: Set[Wallet])(client: Client[IO]): IO[Seq[ZApp]] =
    val addr = wallets.map(_.address.str).mkString(",")
    val nets = Networks.mkString("networks[]=", "&", "")

    for u <- IO.fromEither(Uri.fromString(s"${endpoint.uri}/balances?addresses[]=$addr&$nets"))
        _ <- IO(println(u))
        j <- client.expect[String](GET(u, Auth))
        s  = Source.fromString(j).getLines.filter(LineFilter).toList
        _ <- IO(s.map(_.take(50)).foreach(println))
        x <- IO.fromEither(s.traverse(l => parse(l.dropWhile(_ != '{')).flatMap(_.as[ZApp])))
    yield x

object ZapperApi:

  val DAppMapping = Map(
    DAppId.Gro -> "gro",
    DAppId.Curve -> "curve",
    DAppId.TraderJoe -> "trader-joe",
    DAppId.Wonderland -> "wonderland",
    DAppId.DefiKingdoms -> "defi-kingdoms"
  )

  val LineFilter: String => Boolean = 
    case line if line.contains("data: {}") => false
    case line if line.startsWith("data:")  => true
    case _                                 => false

  val Networks: Seq[String] = Seq (
    "ethereum",
    "binance-smart-chain",
    "avalanche",
    "fantom",
    "polygon",
    "harmony"
  )

  lazy val DAppMappingReverse = DAppMapping.map {
    case (key, value) => value -> key
  }

  def appId(id: String): Option[DAppId] = 
    DAppMappingReverse.get(id)
