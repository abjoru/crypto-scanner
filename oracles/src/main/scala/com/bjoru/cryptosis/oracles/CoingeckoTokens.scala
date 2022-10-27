package com.bjoru.cryptosis.oracles

import cats.effect.IO
import cats.syntax.traverse.given

import io.circe.*

import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.implicits.uri
import org.http4s.circe.CirceEntityDecoder.given

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*

import scala.concurrent.duration.*

class CoingeckoTokens extends TokenApi:

  val geckoUri: Uri = uri"https://api.coingecko.com/api/v3/coins/list?include_platform=true"

  val CACHE_FILE  = cryptosisDirectory(Xdg.Config) </> "tokens.json"
  val MISSED_FILE = cryptosisDirectory(Xdg.Config) </> "unknown-tokens.json"
  val GECKO_FILE  = cryptosisDirectory(Xdg.Cache) </> "coingecko-tokens.json"

  val chainIdMap: Map[String, Chain] = Map(
    "bitcoin"             -> Chain.Bitcoin,
    "ethereum"            -> Chain.Ethereum,
    "solana"              -> Chain.Solana,
    "elrond"              -> Chain.Elrond,
    "binance-smart-chain" -> Chain.Binance,
    "avalanche"           -> Chain.Avalanche,
    "fantom"              -> Chain.Fantom,
    "polygon-pos"         -> Chain.Polygon,
    "polygon"             -> Chain.Polygon,
    "matic"               -> Chain.Polygon,
    "harmony-shard-0"     -> Chain.Harmony,
    "dogechain"           -> Chain.Dogecoin,
    "dogecoin"            -> Chain.Dogecoin,
    "cardano"             -> Chain.Cardano,
    "polkadot"            -> Chain.Polkadot
  )

  def resolve(tokens: Token*)(using Client[IO]): IO[Seq[Token]] = cache.flatMap { ch =>
    val (resolved, unresolved) = tokens.partition(t => ch.contains(t.id))

    if unresolved.isEmpty
      then IO.pure(resolved)
      else missed.flatMap { mh =>
        val (resolved, unresolved) = tokens.partition(t => mh.contains(t.id))
        
        unresolved match 
          case xs if xs.isEmpty => IO.pure(resolved)
          case xs =>
            for res <- geckos.map(gh => xs.partition(t => gh.contains(t.id)))
                _   <- saveIf(CACHE_FILE, res._1, ch)
                _   <- saveIf(MISSED_FILE, res._2, mh)
            yield resolved ++ res._1 ++ res._2
      }
  }

  def lookup(id: Id)(using Client[IO]): IO[Option[Token]] = 
    cache.map(_.get(id)).flatMap {
      case Some(t) => IO.pure(Some(t))
      case None    => geckos.map(_.get(id))
    }

  def bluechip(chain: Chain)(using Client[IO]): IO[Token] = 
    lookup(chain.symbol, chain).flatMap {
      case Some(t) => IO.pure(t)
      case None    => IO.raiseError(Exception(s"Unable to find bluechip token for $chain"))
    }

  def cache: IO[Map[Id, Token]] =
    loadJson[Map[Id, Token]](CACHE_FILE).handleError(_ => Map.empty)

  def missed: IO[Map[Id, Token]] =
    loadJson[Map[Id, Token]](MISSED_FILE).handleError(_ => Map.empty)

  def geckos(using Client[IO]): IO[Map[Id, Token]] =
    if GECKO_FILE.expired(10.days)
      then fetchGeckos
      else loadJson[Map[Id, Token]](GECKO_FILE)

  ///////////////////////
  // Utility functions //
  ///////////////////////

  private def saveIf(f: FilePath, data: Seq[Token], coll: Map[Id, Token]): IO[Unit] =
    if data.isEmpty
      then IO.unit
      else saveJson(f, coll ++ data.map(t => t.id -> t).toMap)

  private def fetchGeckos(using client: Client[IO]): IO[Map[Id, Token]] = 
    for json <- client.expect[Seq[GToken]](geckoUri)
        toks  = processGeckoTokens(json)
        res   = toks._2.map(t => t.id -> t).toMap
        _    <- GECKO_FILE.delete
        _    <- saveJson(GECKO_FILE, res)
        _    <- checkBluechips(toks._1)
    yield res

  private def checkBluechips(tokens: Seq[Token]): IO[Unit] = 
    def saveIfNeeded(cx: Map[Id, Token], nx: Seq[Token]): IO[Unit] =
      if nx.isEmpty
        then IO.unit
        else saveJson(CACHE_FILE, cx ++ nx.map(v => v.id -> v))

    for cx <- cache
        ms  = tokens.filterNot(t => cx.contains(t.id))
        rs <- saveIfNeeded(cx, ms)
    yield ()

  private def processGeckoTokens(tokens: Seq[GToken]): (Seq[Token], Seq[Token]) =
    tokens.foldLeft(Seq.empty[Token], Seq.empty[Token]) {

      // Bluechip
      case ((bcc, acc), GToken(id@"bitcoin", sym@Symbol.Btc, name, _)) =>
        val t = Token(id, name, sym, Chain.Bitcoin, None, 8, Balance.Zero)
        (bcc :+ t, acc :+ t)
      case ((bcc, acc), GToken(id@"dogecoin", sym@Symbol.Doge, name, _)) =>
        val t = Token(id, name, sym, Chain.Dogecoin, None, 8, Balance.Zero)
        (bcc :+ t, acc :+ t)
      case ((bcc, acc), GToken(id@"solana", sym@Symbol.Sol, name, _)) =>
        val t = Token(id, name, sym, Chain.Solana, None, 8, Balance.Zero)
        (bcc :+ t, acc :+ t)
      case ((bcc, acc), GToken(id@"elrond-erd-2", sym@Symbol.Egld, name, _)) =>
        val t = Token(id, name, sym, Chain.Elrond, None, 18, Balance.Zero)
        (bcc :+ t, acc :+ t)
      case ((bcc, acc), GToken(id@"binancecoin", sym@Symbol.Bnb, name, _)) =>
        val t = Token(id, name, sym, Chain.Binance, None, 18, Balance.Zero)
        (bcc :+ t, acc :+ t)
      case ((bcc, acc), GToken(id@"fantom", sym@Symbol.Ftm, name, pfms)) =>
        val base = Token(id, name, sym, Chain.Fantom, None, 18, Balance.Zero)
        val tx = pfms.map(kv => base.copy(chain = kv._1, contract = kv._2)).toSeq :+ base
        (bcc ++ tx, acc ++ tx)

      // MEMO is a receipt for TIME and not available on coingecko
      // it trades at the same price, so use same price id for it.
      case ((bcc, acc), GToken(id@"wonderland", sym, name, pfms)) =>
        val tx = pfms.map(kv => Token(id, name, sym, kv._1, kv._2, 18, Balance.Zero))
        val sx = tx.map(_.withName("Wonderland MEMO").withSymbol(Symbol("memo")))
        bcc -> (acc ++ tx ++ sx)

      // Standard tokens
      case ((bcc, acc), GToken(id, sym, name, pfms)) if pfms.isEmpty =>
        val token = Token(id, name, sym, resolveChain(id), None, 8, Balance.Zero)
        bcc -> (acc :+ token)

      case ((bcc, acc), GToken(id, sym, name, pfms)) =>
        val tx = pfms.map(kv => Token(id, name, sym, kv._1, kv._2, 18, Balance.Zero))
        bcc -> (acc ++ tx.toSeq)
    }

  def resolveChain(geckoId: String): Chain =
    geckoId.split('-').foldLeft(Chain.Unknown) {
      case (acc, str) if acc != Chain.Unknown => acc
      case (_, str) => chainIdMap.get(str).getOrElse(Chain.Unknown)
    }

final case class GToken(
  id:        String,
  symbol:    Symbol, 
  name:      String,
  platforms: Map[Chain, Option[Address]]
)

object GToken:

  given Decoder[GToken] = Decoder.instance { hc =>
    for a <- hc.downField("id").as[String]
        b <- hc.downField("symbol").as[Symbol]
        c <- hc.downField("name").as[String]
        d <- hc.downField("platforms").as[Json].map(decodeMap)
    yield GToken(a, b, c, d)
  }

  private def decodeMap(json: Json): Map[Chain, Option[Address]] =
    def parseMaybeAddress(k: String) = 
      json.hcursor.downField(k).as[Option[Address]].map(k -> _)

    val keys = json.hcursor.keys.map(_.toList).getOrElse(List.empty)
    val ic = keys.traverse(parseMaybeAddress)
    val cm = ic.map(_.map(v => keyToChain(v._1).map(_ -> v._2)).flatten.toMap)

    cm.getOrElse(Map.empty)

  def keyToChain(key: String): Option[Chain] = key match
    case "bitcoin"             => Some(Chain.Bitcoin)
    case "ethereum"            => Some(Chain.Ethereum)
    case "solana"              => Some(Chain.Solana)
    case "elrond"              => Some(Chain.Elrond)
    case "binance-smart-chain" => Some(Chain.Binance)
    case "avalanche"           => Some(Chain.Avalanche)
    case "fantom"              => Some(Chain.Fantom)
    case "polygon-pos"         => Some(Chain.Polygon)
    case "harmony-shard-0"     => Some(Chain.Harmony)
    case "dogechain"           => Some(Chain.Dogecoin)
    case _                     => None
