package com.bjoru.cryptosis

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

import com.bjoru.cryptosis.types.*

object GeckoPricer:

  val baseUri = uri"https://api.coingecko.com/api/v3"

  def fetchPrices(tokens: Seq[Token], client: Client[IO])(env: Env): IO[Env] =
    val tids = tokens.map(_.geckoId).mkString(",")
    for u <- IO.pure(baseUri / "simple" / "price" +? ("ids", tids) +? ("vs_currencies" -> "usd"))
        j <- client.expect(u)(jsonOf[IO, Json])
        r <- IO.fromEither(processPrices(tokens, j))
    yield env.updatePrices(r)

  private def processPrices(tokens: Seq[Token], data: Json): Decoder.Result[Seq[(Id, Price)]] = 
    val c   = data.hcursor
    val ids = c.keys.getOrElse(Iterable.empty).toSeq
    val px  = ids.traverse(gi => c.downField(gi).downField("usd").as[Option[Price]].map(gi -> _))

    px.map { priceList =>
      val lup = priceList.map(kv => kv._1 -> kv._2.getOrElse(Price.Zero)).toMap
      tokens.map(t => t.id -> lup.get(t.geckoId).getOrElse(Price.Zero))
    }
