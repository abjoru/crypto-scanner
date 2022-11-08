package com.bjoru.cryptosis.providers

import cats.effect.IO
import cats.syntax.traverse.*
import cats.syntax.foldable.*

import io.circe.*
import io.circe.parser.parse
import io.circe.syntax.*

import org.http4s.*
import org.http4s.headers.*
import org.http4s.dsl.io.*
import org.http4s.client.*
import org.http4s.client.dsl.io.*
import org.http4s.circe.*
import org.http4s.circe.CirceEntityDecoder.given

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*

import scala.io.Source

class Zapper(ep: Endpoint) extends ProviderApi(ProviderName.Zapper):

  val Auth = ep.apiKey.map(k => Authorization(BasicCredentials(k, "")))
  val Acc  = Header("Accept", "*/*")

  val supportedChains = Seq(
    Chain.Ethereum,
    Chain.Binance,
    Chain.Avalanche,
    Chain.Fantom,
    Chain.Polygon,
    Chain.Harmony
  )

  val lineFilter: String => Boolean = 
    case line if line.contains("data: {}") => false
    case line if line.startsWith("data:")  => true
    case _                                 => false

  protected def sync(wallets: Seq[Wallet])(using client: Client[IO]): IO[SyncResponse] =
    for url <- uri(wallets.filter(w => supportedChains.contains(w.chain)))
        key <- IO.fromOption(Auth)(Exception(f"$name%-15s: missing api-key!"))
        _   <- putStrLn(f"$name%-15s: synchronizing wallets...")
        txt <- client.expect[String](GET(url, Auth, Acc))(using EntityDecoder.text[IO])
        res <- processTextReply(txt)
    yield ZapperResponse(res)

  private def uri(wallets: Seq[Wallet]): IO[Uri] =
    val base = (ep.uri / "balances").toString
    val params = wallets.map(_.address.toString).mkString("&addresses[]=")
    IO.fromEither(Uri.fromString(s"${base}?addresses[]=$params&bundled=false"))

  private def processTextReply(txt: String): IO[Seq[SyncData]] = 
    for lines <- IO.pure(Source.fromString(txt).getLines.filter(lineFilter).toSeq)
        jsons <- IO.fromEither(lines.traverse(v => parse(v.dropWhile(_ != '{'))))
        idx   <- indexMap(jsons)
    yield idx.map(kv => SyncData(kv._1._3, kv._1._1, Seq(kv._2))).toSeq

  private def indexMap(data: Seq[Json]): IO[Map[(String, Chain, Address), Json]] =
    val indices = data.traverse { json =>
      for a <- IO.fromEither(json.hcursor.downField("appId").as[String])
          b <- IO.fromEither(json.hcursor.downField("network").as[Chain])
          c <- IO.fromEither(json.hcursor.downField("addresses").as[Seq[Address]].map(_.head))
      yield (a, b, c) -> json
    }

    indices.map(_.toMap)

class ZapperResponse(val data: Seq[SyncData]) extends FoldableSyncResponse:

  import decoders.*
  import ZapperDecoders.given

  val provider: ProviderName = ProviderName.Zapper

  def withData(extras: Seq[SyncData]): SyncResponse = ZapperResponse(data ++ extras)

  def syncWallet(wallet: Wallet)(using Client[IO]) =
    case SyncData(_, _, jsons) => 
      for res <- jsons.traverse(_.as[ZapperResult]).toSIO
          grp  = reduce(res)
          tok  = grp.map(_.tokens).reduce(_ ++ _)
          dap  = grp.map(_.apps).reduce(_ ++ _)
          rs1 <- State.resolveAllWithPrice(tok.map(v => v.token -> v.price))
          rs2 <- State.resolveAllApps(dap)
      yield wallet.addBalances(rs1: _*).addBalances(rs2: _*)

  private def reduce(data: Seq[ZapperResult]): Seq[ZapperResult] =
    data.groupBy(_.chain).foldLeft(Seq.empty[ZapperResult]) {
      case (acc, (_, xs)) => acc :+ xs.reduce { (a, b) => 
        a.copy(tokens = a.tokens ++ b.tokens, apps = a.apps ++ b.apps)
      }
    }
