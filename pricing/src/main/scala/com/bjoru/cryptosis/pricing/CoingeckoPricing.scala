package com.bjoru.cryptosis.pricing

import cats.effect.IO
import cats.syntax.traverse.given

import io.circe.*

import org.http4s.implicits.uri
import org.http4s.client.Client
import org.http4s.circe.*

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*

final case class TokenPrice(
  token: Token,
  price: Option[Price]
)

class CoingeckoPricing(cache: Map[Id, TokenPrice]) extends Pricing:

  val geckoUri = uri"https://api.coingecko.com/api/v3/simple/price"

  def priceOf(token: Token): Price = 
    cache.get(token.id).flatMap(_.price).getOrElse(Price.Zero)

  def valueOf(token: Token): Price = ???

  def valueOf(app: Defi): Price = app match
    case Defi.Stake(_, _, _, l)   => l.map(valueOf).sum
    case Defi.Farm(_, _, _, l, c) => l.map(valueOf).sum + c.map(valueOf).sum
    case Defi.Pool(_, _, _, l, p) => l.map(valueOf).sum + valueOf(p)

  def valueOf(wallet: Wallet): Price =
    val priceList = wallet.balances.map {
      case (_, t: Token) => valueOf(t)
      case (_, d: Defi)  => valueOf(d)
    }

    priceList.sum

  def valueOf(exchange: Exchange): Price =
    val priceList = exchange.balances.map {
      case (_, t: Token) => valueOf(t)
      case (_, d: Defi)  => valueOf(d)
    }

    priceList.sum

  def register(tokens: Token*): Pricing = 
    val newCache = tokens.foldLeft(cache) {
      case (acc, t) => acc.updatedWith(t.id) {
        case Some(_) => None
        case None    => Some(TokenPrice(t, None))
      }
    }

    CoingeckoPricing(newCache)

  def registerPrices(tokens: (Token, Price)*): Pricing =
    val newCache = tokens.foldLeft(cache) {
      case (acc, (t, p)) => acc.updated(t.id, TokenPrice(t, Some(p)))
    }

    CoingeckoPricing(newCache)

  def syncPrices(using Client[IO]): IO[Pricing] =
    for cand <- IO.pure(cache.filter(_._2.price.isEmpty).map(_._2.token.geckoId))
        res  <- if cand.nonEmpty then syncCandidates(cand.toSeq) else IO.pure(this)
    yield res

  private def syncCandidates(gids: Seq[String])(using client: Client[IO]): IO[Pricing] =
    for url <- IO.pure(geckoUri +? ("ids", gids.mkString(",")) +? ("vs_currencies", "usd"))
        jsn <- client.expect[Json](url)
        ids <- IO.pure(jsn.hcursor.keys.getOrElse(Iterable.empty).toSeq)
        gpr <- IO.fromEither(ids.traverse(g => jsn.hcursor.downField(g).downField("usd").as[Option[Price]].map(g -> _)))
    yield updateWith(gpr)

  private def updateWith(prices: Seq[(String, Option[Price])]): Pricing =
    val newCache = prices.foldLeft(cache) {
      case (acc, (gid, Some(price))) =>
        val maybeTokenP = acc.find(_._2.token.geckoId == gid)
        val mCache = maybeTokenP.map(v => acc.updated(v._1, v._2.copy(price = Some(price))))
        mCache.getOrElse(acc)
      case (acc, _) => acc
    }

    CoingeckoPricing(newCache)
