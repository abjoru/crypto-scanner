package com.bjoru.cryptos.api

import cats.effect.IO

import io.circe.Json

import org.http4s.Uri
import org.http4s.client.*
import org.http4s.circe.*

import com.bjoru.cryptos.types.*

import scala.util.{Try, Success, Failure}

import java.nio.file.Path

class ZapperApi extends CryptoApi:

  val supportedChains = Seq(
    Chain.Ethereum,
    Chain.Binance,
    Chain.Avalanche,
    Chain.Fantom,
    Chain.Polygon,
    Chain.Harmony
  )

  def doSyncWallets(wallets: Seq[Wallet], client: Client[IO]): IO[Seq[Wallet]] = ???

  def supportedBalances(wallets: Set[Wallet])(client: Client[IO]): IO[Json] =
    val addr = wallets.map(_.address.str).mkString(",")

    for u <- IO.fromEither(Uri.fromString(s"https://api.zapper.fi/v2/apps/balances/supported?addresses[]=$addr"))
        j <- client.expect(u)(jsonOf[IO, Json])
    yield j

  def appBalance(app: DAppId, wallets: Set[Wallet])(client: Client[IO]): IO[Json] =
    val addr = wallets.map(_.address.str).mkString(",")
    val id   = ZapperApi.DAppMapping(app)

    for u <- IO.fromEither(Uri.fromString(s"https://api.zapper.fi/v2/apps/$id/balances?addresses[]=$addr"))
        j <- client.expect(u)(jsonOf[IO, Json])
    yield j

object ZapperApi:

  val DAppMapping = Map(
    DAppId.Gro -> "gro",
    DAppId.Curve -> "curve",
    DAppId.TraderJoe -> "trader-joe",
    DAppId.Wonderland -> "wonderland",
    DAppId.DefiKingdoms -> "defi-kingdoms"
  )

  lazy val DAppMappingReverse = DAppMapping.map {
    case (key, value) => value -> key
  }

  def appId(id: String): Option[DAppId] = 
    DAppMappingReverse.get(id)
