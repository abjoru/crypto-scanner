package com.bjoru.cryptosis

import cats.effect.IO

import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.client.*
import org.http4s.client.dsl.io.*
import org.http4s.circe.*
import org.http4s.circe.CirceEntityDecoder.given

import com.bjoru.cryptosis.config.GToken
import com.bjoru.cryptosis.types.*

import scala.util.Try

class Env(val registry: TokenRegistry):

  def updateToken(token: Token): Env =
    Env(registry.updateToken(token))

  def updatePrice(id: Id, price: Price): Env =
    Env(registry.updatePrice(id, price))

  def updatePrices(prices: Seq[(Id, Price)]): Env =
    Env(registry.updatePrices(prices))

  def findTokenById(id: Id): Option[Token] =
    registry.findById(id)

  def findTokenByContract(contract: Address): Option[Token] =
    registry.findByContract(contract)

  def resolveToken(token: Token): Option[Token] =
    registry.resolveToken(token)

  def bluechipToken(chain: Chain): Try[Token] =
    registry.bluechipFor(chain)

  def priceOf(token: Token): Token = 
    registry.priceOf(token)

  def resolveAndUpdate(token: Token): (Env, Token) =
    val (tr, t2) = registry.resolveAndUpdate(token)
    Env(tr) -> t2

  def resolveAndUpdateAll(tokens: Seq[Token]): (Env, Seq[Token]) =
    val (tr, tx) = registry.resolveAndUpdateAll(tokens)
    Env(tr) -> tx

  def saveEnv(cacheFile: FilePath): IO[Unit] =
    registry.saveCache(cacheFile)

object Env:

  def loadEnv(cacheFile: FilePath)(client: Client[IO]): IO[Env] = 
    for glist <- fetchTokenList(client)
        reg   <- TokenRegistry(cacheFile, glist)
    yield Env(reg)

  private def fetchTokenList(client: Client[IO]): IO[Seq[Token]] =
    for gt <- IO.fromEither(Uri.fromString("https://api.coingecko.com/api/v3/coins/list?include_platform=true"))
        dx <- client.expect[Seq[GToken]](gt)
    yield processGeckos(dx)

  private def processGeckos(gtokens: Seq[GToken]): Seq[Token] = 
    val listOfLists = gtokens.map { 
      case GToken(id@"bitcoin", sym@Symbol.Btc, name, _) =>
        Seq(Token(id, name, sym, Chain.Bitcoin, 8, None))
      case GToken(id@"dogecoin", sym@Symbol.Doge, name, _) =>
        Seq(Token(id, name, sym, Chain.Dogecoin, 8, None))
      case GToken(id@"solana", sym@Symbol.Sol, name, _) =>
        Seq(Token(id, name, sym, Chain.Solana, 8, None))
      case GToken(id@"elrond-erd-2", sym@Symbol.Egld, name, _) =>
        Seq(Token(id, name, sym, Chain.Elrond, 18, None))
      case GToken(id, sym, name, pfms) if pfms.isEmpty =>
        Seq(Token(id, name, sym, resolveChain(id), None))
      case GToken(id, sym, name, pfms) =>
        pfms.map(kv => Token(id, name, sym, kv._1, kv._2))
    }

    listOfLists.flatten.filterNot(_.chain == Chain.Unknown)

  private def resolveChain(id: String): Chain =
    val chainMap = Map(
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

    id.split('-').foldLeft(Chain.Unknown) {
      case (acc, str) if acc != Chain.Unknown => acc
      case (_, str) => chainMap.get(str).getOrElse(Chain.Unknown)
    }
