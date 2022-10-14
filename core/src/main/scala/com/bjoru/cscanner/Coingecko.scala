package com.bjoru.cscanner

import cats.syntax.traverse.given
import cats.effect.IO

import io.circe.*

import org.http4s.dsl.io.*
import org.http4s.client.*
import org.http4s.client.dsl.io.*
import org.http4s.circe.*
import org.http4s.circe.CirceEntityDecoder.given
import org.http4s.implicits.uri
import org.http4s.ember.client.*

import com.bjoru.cscanner.{*, given}
import com.bjoru.cscanner.config.loadJsonFile
import com.bjoru.cscanner.types.*

import java.nio.file.Path

class Coingecko(cfgDir: Path):

  case class Gecko(id: String, symbol: Symbol, name: String)

  given Decoder[Gecko] = Decoder.forProduct3("id", "symbol", "name")(Gecko.apply)

  val Uri = uri"https://api.coingecko.com/api/v3"

  val clientR = EmberClientBuilder.default[IO].build

  def symbolToIdMap: IO[Map[Symbol, String]] =
    loadJsonFile[Seq[Gecko]](cfgDir </> "coingecko.json").map(_.map(kv => kv._2 -> kv._1).toMap)

  def priceFor(symbols: Set[Symbol]): IO[Map[Symbol, Double]] = 
    for idMap <- symbolToIdMap
        ids    = symbols.map(idMap.get).flatten.mkString(",")
        json  <- clientR.use(_.expect[Json](Uri / "simple" / "price" +? ("ids" -> ids) +? ("vs_currencies" -> "usd")))
        data   = parsePrice(json, idMap)
    yield data.toMap

  private def parsePrice(json: Json, symbolMap: Map[Symbol, String]): Iterable[(Symbol, Double)] =
    val cursor  = json.hcursor
    val reverse = for ((k, v) <- symbolMap) yield (v, k)
    val ids     = cursor.keys.getOrElse(Iterable.empty).map { id =>
      reverse.get(id) match 
        case Some(sym) => cursor.downField(id).downField("usd").as[Double].map(sym -> _)
        case None      => Left(DecodingFailure("No such symbol", cursor.history))
    }

    ids.collect { case Right(v) => v }
