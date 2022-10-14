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

import com.github.vickumar1981.stringdistance.StringDistance.*
import com.github.vickumar1981.stringdistance.given

import java.nio.file.Path

class Coingecko(cfgDir: Path):

  import Balance.TokenBalance

  case class Gecko(id: String, symbol: Symbol, name: String)

  given Decoder[Gecko] = Decoder.forProduct3("id", "symbol", "name")(Gecko.apply)

  val Uri = uri"https://api.coingecko.com/api/v3"

  val clientR = EmberClientBuilder.default[IO].build

  def geckoIndex: IO[Seq[Gecko]] =
    loadJsonFile[Seq[Gecko]](cfgDir </> "coingecko.json")


  def priceFor(tokens: Seq[TokenBalance]): IO[Seq[TokenBalance]] = 
    for idLst <- geckoIndex
        gecks  = tokens.map(t => findBySymbol(idLst, t).map(t -> _)).flatten
        ids    = gecks.map(_._2.id).mkString(",")
        json  <- clientR.use(_.expect[Json](Uri / "simple" / "price" +? ("ids" -> ids) +? ("vs_currencies" -> "usd")))
        res    = parsePrice(json, gecks)
    yield res

  private def findBySymbol(index: Seq[Gecko], tb: TokenBalance): Option[Gecko] =
    index.toList.filter(_.symbol == tb.token.symbol) match
      case Nil => None
      case h :: Nil => Some(h)
      case other =>
        val matches = index.filter(_.symbol == tb.token.symbol)
        val score = matches.map(v => v -> Cosine.score(v.name, tb.token.name))
        val selected = score.maxBy(_._2)
        println(s"Selected ${selected._1} from ${matches.size} matches for ${tb.token.symbol} based on score: ${selected._2}")
        Some(selected._1)
    
  private def parsePrice(json: Json, gecks: Seq[(TokenBalance, Gecko)]): Seq[TokenBalance] =
    val cursor     = json.hcursor
    val priceData  = cursor.keys.getOrElse(Iterable.empty).map { id =>
      cursor.downField(id).downField("usd").as[Double].map(id -> _).toOption
    }.flatten.toMap

    gecks.map { (t, g) =>
      priceData.get(g.id) match
        case Some(price) => t.withPrice(price)
        case None        => t
    }
