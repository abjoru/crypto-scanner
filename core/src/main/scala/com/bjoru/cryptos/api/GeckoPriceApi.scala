package com.bjoru.cryptos.api

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

import com.bjoru.cryptos.config.loadJsonFile
import com.bjoru.cryptos.types.*
import com.bjoru.cryptos.utils.{*, given}

import com.github.vickumar1981.stringdistance.StringDistance.*
import com.github.vickumar1981.stringdistance.given

import java.nio.file.Path

final case class Gecko(id: String, name: String, symbol: Symbol)

object Gecko:

  given Decoder[Gecko] = Decoder.forProduct3("id", "name", "symbol")(Gecko.apply)

class GeckoPricer(prices: Seq[(Token, Double)]):

  def price(t: Token): Token = prices.find(_._1 == t) match
    case Some((_, price)) => t.withPrice(price)
    case None        => t

class GeckoPriceApi(cfgDir: Path):

  val baseUri = uri"https://api.coingecko.com/api/v3"

  private def geckoIndex: IO[Seq[Gecko]] =
    loadJsonFile[Seq[Gecko]](cfgDir </> "coingecko.json")

  private def matchTokens(tokens: Seq[Token], index: Seq[Gecko]): Seq[(Token, Gecko)] =
    val indexGroups = index.groupBy(_.symbol)

    val result = tokens.map { token =>
      indexGroups.get(token.symbol).flatMap {
        case Nil => None
        case h :: Nil => Some(token -> h)
        case many =>
          val scores = many.map(v => v -> Cosine.score(v.name, token.name))
          Some(token -> scores.maxBy(_._2)._1)
      }
    }

    result.flatten

  private def mkTable(json: Json, matches: Seq[(Token, Gecko)]) = 
    val cursor    = json.hcursor
    val priceData = cursor.keys.getOrElse(Iterable.empty).map { id =>
      cursor.downField(id).downField("usd").as[Double].map(id -> _).toOption
    }.flatten.toMap

    matches.map { (t, g) =>
      priceData.get(g.id).map(t -> _)
    }.flatten

  def priceTable(tokens: Seq[Token])(client: Client[IO]): IO[Seq[(Token, Double)]] =
    for idx <- geckoIndex
        mth  = matchTokens(tokens, idx)
        arg  = mth.map(_._2.id).mkString(",")
        jsn <- client.expect[Json](baseUri / "simple" / "price" +? ("ids" -> arg) +? ("vs_currencies" -> "usd"))
        res  = mkTable(jsn, mth)
    yield res

  def pricer(tokens: Seq[Token])(client: Client[IO]): IO[GeckoPricer] =
    priceTable(tokens)(client).map(GeckoPricer(_))
