package com.bjoru.cryptosis.oracles

import cats.effect.IO
import cats.syntax.traverse.*

import io.circe.*
import io.circe.syntax.*

import org.http4s.*
import org.http4s.client.Client
import org.http4s.implicits.*
import org.http4s.circe.CirceEntityDecoder.given

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*

class CoingeckoOracle extends Oracle:

  import State.given_Encoder_Token

  val priceUri = uri"https://api.coingecko.com/api/v3/simple/price"

  val tokenUri: Uri = uri"https://api.coingecko.com/api/v3/coins/list?include_platform=true"

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
    "polkadot"            -> Chain.Polkadot,
    "smartbck"            -> Chain.SmartBCH
  )

  def fetchPrices(tokens: Seq[Token])(using client: Client[IO]): IO[Map[Id, Price]] =
    for url <- IO.pure(priceUri +? ("ids", tokens.map(_.geckoId).mkString(",")) +? ("vs_currencies", "usd"))
        jsn <- client.expect[Json](url)
        ids  = jsn.hcursor.keys.getOrElse(Iterable.empty).toSeq
        gpr <- ids.traverse(g => jsn.hcursor.downField(g).downField("usd").as[Option[Price]].map(g -> _)).toIO
        mpr  = gpr.toMap
        tpr  = tokens.map(t => t.id -> mpr.get(t.geckoId).flatten)
      yield tpr.map((a, b) => a -> b.getOrElse(Price.Zero)).toMap

  def fetchTokens(file: FilePath)(using client: Client[IO]): IO[Map[Id, Token]] = 
    for data <- client.expect[Seq[GToken]](tokenUri).withErrorHandler
        toks  = processTokens(data)
        _    <- saveJson(file, toks).withErrorHandler
    yield toks.map(t => t.id -> t).toMap

  private def processTokens(tokens: Seq[GToken]): Seq[Token] =
    tokens.foldLeft(Seq.empty[Token]) {

      // Bluechip
      case (acc, GToken(id@"bitcoin", name, sym@Symbol.Btc, _)) =>
        acc :+ Token(id, name, sym, Chain.Bitcoin, None, 8, Balance.Zero)
      case (acc, GToken(id@"bitcoin-cash", name, sym@Symbol.Bch, _)) =>
        acc :+ Token(id, name, sym, Chain.BitcoinCash, None, 8, Balance.Zero)
      case (acc, GToken(id@"bitcoin-cash-sv", name, sym@Symbol.Bsv, _)) =>
        acc :+ Token(id, name, sym, Chain.BitcoinSV, None, 8, Balance.Zero)
      case (acc, GToken(id@"dogecoin", name, sym@Symbol.Doge, _)) =>
        acc :+ Token(id, name, sym, Chain.Dogecoin, None, 8, Balance.Zero)
      case (acc, GToken(id@"solana", name, sym@Symbol.Sol, _)) =>
        acc :+ Token(id, name, sym, Chain.Solana, None, 8, Balance.Zero)
      case (acc, GToken(id@"elrond-erd-2", name, sym@Symbol.Egld, _)) =>
        acc :+ Token(id, name, sym, Chain.Elrond, None, 18, Balance.Zero)
      case (acc, GToken(id@"binancecoin", name, sym@Symbol.Bnb, _)) =>
        acc :+ Token(id, name, sym, Chain.Binance, None, 18, Balance.Zero)
      case (acc, GToken(id@"fantom", name, sym@Symbol.Ftm, pfms)) =>
        val base = Token(id, name, sym, Chain.Fantom, None, 18, Balance.Zero)
        val tx = pfms.map(kv => base.copy(chain = kv._1, contract = kv._2)).toSeq :+ base
        acc ++ tx

      // MEMO is a receipt for TIME and not available on coingecko
      // it trades at the same price, so use same price id for it.
      case (acc, GToken(id@"wonderland", name, sym, pfms)) =>
        val tx = pfms.map(kv => Token(id, name, sym, kv._1, kv._2, 18, Balance.Zero))
        val sx = tx.map(_.withName("Wonderland MEMO").withSymbol(Symbol("memo")))
        acc ++ tx ++ sx

      // Standard tokens
      case (acc, GToken(id, name, sym, pfms)) if pfms.isEmpty =>
        acc :+ Token(id, name, sym, resolveChain(id), None, 8, Balance.Zero)

      case (acc, GToken(id, name, sym, pfms)) =>
        acc ++ pfms.map(kv => Token(id, name, sym, kv._1, kv._2, 18, Balance.Zero)).toSeq
    }
  
  private def resolveChain(gid: String): Chain =
    gid.split('-').foldLeft(Chain.Unknown) {
      case (acc, _) if acc != Chain.Unknown => acc
      case (acc, str) => chainIdMap.get(str).getOrElse(acc)
    }
