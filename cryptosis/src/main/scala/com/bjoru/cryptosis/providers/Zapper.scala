package com.bjoru.cryptosis.providers

import cats.effect.IO
import cats.syntax.foldable.given
import cats.syntax.traverse.given

import io.circe.*
import io.circe.parser.parse
import io.circe.syntax.{*, given}

import org.http4s.*
import org.http4s.headers.*
import org.http4s.dsl.io.*
import org.http4s.client.*
import org.http4s.client.dsl.io.*
import org.http4s.circe.*
import org.http4s.circe.CirceEntityDecoder.given
import org.http4s.implicits.uri

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*
import com.bjoru.cryptosis.syntax.circe.*
import com.bjoru.cryptosis.providers.decoders.ZapperDecoder

import scala.io.Source

class Zapper(ep: Endpoint) extends ProviderApi:

  val Auth = Authorization(BasicCredentials(ep.apiKey, ""))

  val supportedChains = Seq(
    Chain.Ethereum,
    Chain.Binance,
    Chain.Avalanche,
    Chain.Fantom,
    Chain.Polygon,
    Chain.Harmony
  )

  def zapperUri(wallets: Seq[Wallet]) = 
    ep.uri / "balances" +? ("addresses[]", wallets.map(_.address.str).mkString(",")) +? ("bundled", false)

  protected def doSync(wallets: Seq[Wallet])(using client: Client[IO]): IO[Seq[Wallet]] =
    for u  <- IO.pure(zapperUri(wallets))
        _  <- putStrLn("zapper: making call...")
        s  <- client.expect[String](GET(u, Auth))(using EntityDecoder.text[IO])
        _  <- putStrLn("zapper: received response, parsing result...")
        l   = Source.fromString(s).getLines.filter(Zapper.LineFilter).toList
        j  <- IO.fromEither(l.traverse(v => parse(v.dropWhile(_ != '{'))))
        _  <- putStrLn(s"zapper: processing ${j.size} items...")
        da <- defiApps(j, wallets)
        ta <- tokenApps(j, wallets)

        r1 <- ZapperDecoder.decodeDefiApps(da)
        r2 <- ZapperDecoder.decodeTokenApps(ta)
    yield Wallet.mergeWallets(r1, r2)

  def indexMap(data: Seq[Json]): IO[Map[(String, Chain, Address), Json]] =
    val indexes = data.traverse { json =>
      for a <- (json <\> "appId").asIO[String]
          b <- (json <\> "network").asIO[Chain]
          c <- (json <\> "addresses").asIO[Seq[Address]].map(_.head)
      yield (a, b, c) -> json
    }

    indexes.map(_.toMap)

  def tokenApps(data: Seq[Json], wallets: Seq[Wallet]) =
    appGroups(data, wallets)(_.filter(_._1._1 == "tokens"))

  def defiApps(data: Seq[Json], wallets: Seq[Wallet]) =
    appGroups(data, wallets)(_.filterNot(_._1._1 == "tokens"))

  def appGroups(data: Seq[Json], wallets: Seq[Wallet])(
    f: Map[(String, Chain, Address), Json] => Map[(String, Chain, Address), Json]
  ): IO[Map[Wallet, Seq[Json]]] =
    val indexes = indexMap(data).map(f)
    val walletMap = wallets.map(_ -> Seq.empty[Json]).toMap

    indexes.map(_.foldLeft(walletMap) { 
      case (wx, ((_, c, a), json)) => 
        wx.find(_._1.address == a) match
          case Some((w, d)) => wx.updated(w, d :+ json)
          case None    => wx
    })

object Zapper:

  val LineFilter: String => Boolean =
    case line if line.contains("data: {}") => false
    case line if line.startsWith("data:")  => true
    case _                                 => false
