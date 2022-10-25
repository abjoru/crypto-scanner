package com.bjoru.cryptosis

import cats.effect.IO
import cats.syntax.foldable.given
import cats.syntax.traverse.given

import io.circe.*

import org.http4s.Uri
import org.http4s.client.*
import org.http4s.circe.*
import org.http4s.circe.CirceEntityDecoder.given
import org.http4s.implicits.uri

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*
import com.bjoru.cryptosis.syntax.circe.*
import com.bjoru.cryptosis.config.GToken
import com.bjoru.cryptosis.utils.CoingeckoMapper

import scala.util.{Try, Success, Failure}
import scala.concurrent.duration.*

object Env:

  val cacheDir = getXdgDirectory(Xdg.Cache) </> "cryptosis"

  val CACHE_TOKEN_FILE = cacheDir </> "cryptosis-tokens.json"
  val GECKO_TOKEN_FILE = cacheDir </> "coingecko-tokens.json"
  val GECKO_PRICE_FILE = cacheDir </> "coingecko-prices.json"

  val priceUri = uri"https://api.coingecko.com/api/v3/simple/price"

  def priceOf(token: Token)(using Client[IO]): IO[Price] = 
    tokenPrices.flatMap { prices =>
      prices.get(token.id) match
        case Some(p) => IO.pure(p)
        case None    => // load price for token and update cache
          for a <- fetchCoingeckoPrices((Seq(token)))
              _ <- saveJsonFile(GECKO_PRICE_FILE, prices ++ a)
              r <- IO.fromOption(a.headOption)(Exception(s"No price for $token"))
          yield r._2
    }

  def priced(tokens: Token*)(using Client[IO]): IO[Seq[Token]] =
    for priced  <- tokenPrices
        missing  = tokens.filterNot(t => priced.contains(t.id))
        fetched <- fetchCoingeckoPrices(missing)
        index    = priced ++ fetched
        _       <- saveJsonFile(GECKO_PRICE_FILE, index)
    yield tokens.map(t => index.get(t.id).map(t.withPrice(_)).getOrElse(t))

  def bluechipToken(chain: Chain)(using Client[IO]): IO[Token] =
    CoingeckoMapper.bluechipSymbolTable.get(chain.symbol) match
      case Some(gid) =>
        val maybeTok = cachedTokens.map(_.values.find(_.geckoId == gid)).flatMap {
          case Some(t) => IO.pure(Some(t))
          case None => coingeckoTokens.map(_.find(_.geckoId == gid))
        }

        maybeTok.flatMap {
          case Some(t) => IO.pure(t)
          case None    => IO.raiseError(Exception(s"No bluechip token for $chain"))
        }
      case None => IO.raiseError(Exception(s"No bluechip token for $chain"))

  def findById(id: Id): IO[Token] =
    cachedTokens.map(v => v(id))

  def findByGeckoId(gid: String)(using Client[IO]): IO[Token] = 
    coingeckoTokens.flatMap { lst =>
      lst.find(_.geckoId == gid) match
        case Some(t) => IO.pure(t)
        case None    => IO.raiseError(Exception(s"No such token: gid=$gid"))
    }

  def findBySymbol(symbol: Symbol, chain: Chain, nameHint: String = ""): IO[Token] =
    def find(tx: List[Token]) = (tx, nameHint) match
      case (Nil, _)               => None
      case (h :: Nil, _)          => Some(h)
      case (grp, n) if n.nonEmpty => CoingeckoMapper.byName(n, grp)
      case (h :: _, _)            => Some(h) // fallback head
     
    for cached <- cachedTokens
        tokens  = cached.values.filter(v => v.symbol == symbol && v.chain == chain).toList
        result <- IO.fromOption(find(tokens))(Exception(s"Token not found: $symbol"))
        _      <- updateCache(result, cached)
    yield result

  def findByContract(contract: Address)(using Client[IO]): IO[Token] =
    def check(t: Option[Token]) = t match
      case Some(v) => IO.pure(v)
      case None => coingeckoTokens.flatMap { tx =>
        val res = tx.find(_.contract.map(_ == contract).getOrElse(false))
        IO.fromOption(res)(Exception(s"No such token address: $contract"))
      }

    for cached <- cachedTokens
        mtok    = cached.values.find(_.contract.map(_ == contract).getOrElse(false))
        result <- check(mtok)
        _      <- updateCache(result, cached)
    yield result

  def resolveExchangeTokens(tokens: Seq[Token])(using Client[IO]): IO[Seq[Token]] =
    for rs <- tokens.traverse(t => findBySymbol(t.symbol, t.chain, t.name))
        ch <- cachedTokens
        _  <- updateCacheForAll(rs, ch)
    yield rs

  def resolveToken(token: Token)(using Client[IO]): IO[Token] =
    for cached <- cachedTokens
        geckos <- coingeckoTokens
        result <- internalResolveToken(token, cached, geckos)
    yield result

  def resolveTokens(tokens: Seq[Token])(using Client[IO]): IO[Seq[Token]] =
    for cached <- cachedTokens
        geckos <- coingeckoTokens
        result <- tokens.distinct.traverse(internalResolveToken(_, cached, geckos))
    yield result

  private def internalResolveToken(token: Token, cache: Map[Id, Token], geckos: Seq[Token]): IO[Token] =
    val rs = if cache.contains(token.id)
               then IO.pure(token)
               else resolveFromGeckos(token, geckos)

    rs.flatMap(t => updateCache(t, cache) >> IO.pure(t))

  private def updateCache(t: Token, cache: Map[Id, Token]): IO[Unit] =
    if cache.contains(t.id)
      then IO.unit
      else saveJsonFile(CACHE_TOKEN_FILE, cache.values.toSeq :+ t)

  private def updateCacheForAll(tx: Seq[Token], cache: Map[Id, Token]): IO[Unit] =
    val missing = tx.filterNot(t => cache.contains(t.id))

    if missing.nonEmpty
      then saveJsonFile(CACHE_TOKEN_FILE, cache.values.toSeq ++ missing)
      else IO.unit

  private def updatePrices(tx: Seq[Token], prices: Map[Id, Price]): IO[Unit] =
    val newPrices = tx.filter(t => !prices.contains(t.id))
    if newPrices.isEmpty
      then IO.unit
      else saveJsonFile(GECKO_PRICE_FILE, prices)

  private def cachedTokens: IO[Map[Id, Token]] =
    if CACHE_TOKEN_FILE.exists
      then loadJsonFile[Seq[Token]](CACHE_TOKEN_FILE).map(_.map(t => t.id -> t).toMap)
      else IO.pure(Map.empty[Id, Token])

  private def coingeckoTokens(using Client[IO]): IO[Seq[Token]] =
    if GECKO_TOKEN_FILE.expired(30.days)
      then fetchCoingeckoTokens
      else loadJsonFile[Seq[Token]](GECKO_TOKEN_FILE)

  private def tokenPrices: IO[Map[Id, Price]] = 
    if GECKO_PRICE_FILE.expired(1.hour)
      then IO.pure(Map.empty[Id, Price])
      else loadJsonFile[Map[Id, Price]](GECKO_PRICE_FILE)

  private def fetchCoingeckoTokens(using client: Client[IO]): IO[Seq[Token]] =
    for gt <- IO.fromEither(Uri.fromString("https://api.coingecko.com/api/v3/coins/list?include_platform=true"))
        dx <- client.expect[Seq[GToken]](gt)
        rx  = CoingeckoMapper.processGeckos(dx)
        _   = GECKO_TOKEN_FILE.delete
        _  <- saveJsonFile(GECKO_TOKEN_FILE, rx)
    yield rx

  private def fetchCoingeckoPrices(tokens: Seq[Token])(using client: Client[IO]): IO[Map[Id, Price]] =
    def processPrices(tx: Seq[Token], data: Json): Decoder.Result[Seq[(Id, Price)]] =
      val hc = data.hcursor
      val ids = hc.keys.getOrElse(Iterable.empty).toSeq
      val px  = ids.traverse(gi => (hc <\> gi <\> "usd").as[Option[Price]].map(gi -> _))

      px.map { priceList =>
        val lup = priceList.map(v => v._1 -> v._2.getOrElse(Price.Zero)).toMap
        tx.map(t => t.id -> lup.get(t.geckoId).getOrElse(Price.Zero))
      }

    val tids = tokens.map(_.geckoId).mkString(",")

    for url <- IO.pure(priceUri +? ("ids", tids) +? ("vs_currencies", "usd"))
        jsn <- client.expect(url)(jsonOf[IO, Json])
        res <- IO.fromEither(processPrices(tokens, jsn))
    yield res.toMap

  private def resolveFromGeckos(token: Token, geckos: Seq[Token]): IO[Token] =
    val mTok = token.contract match
      case Some(c) => CoingeckoMapper.byContract(c, geckos)
      case None    => None

    mTok match
      case Some(t) => IO.pure(t.base(token))
      case None    =>
        val grps = geckos.filter(t => t.symbol == token.symbol && t.chain == token.chain)
        IO.fromTry(CoingeckoMapper.bestMatched(token, grps)).map(_.base(token))
