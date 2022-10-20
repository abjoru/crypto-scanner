package com.bjoru.cryptosis

import cats.effect.IO
import cats.syntax.show.given

import com.bjoru.cryptosis.types.*

import com.github.vickumar1981.stringdistance.StringDistance.*
import com.github.vickumar1981.stringdistance.given

import scala.util.{Try, Success, Failure}

final case class TokenRegistry(
  geckoTokens: Seq[Token],
  geckoPrices: Map[String, Price],
  persistentCache: Map[Id, Token]
)

object TokenRegistry:

  extension (r: TokenRegistry)

    def updateToken(token: Token): TokenRegistry =
      r.copy(persistentCache = r.persistentCache.updated(token.id, token))

    def updatePrice(geckoId: String, price: Price): TokenRegistry =
      r.copy(geckoPrices = r.geckoPrices.updated(geckoId, price))

    def updatePrices(prices: Seq[(String, Price)]): TokenRegistry =
      prices.foldLeft(r) { case (acc, (i, p)) => acc.updatePrice(i, p) }

    def findById(id: Id): Option[Token] = r.persistentCache.get(id)

    def findByContract(contract: Address): Option[Token] =
      val maybeCached = r.persistentCache.find(_._2.contract.map(_ == contract).getOrElse(false))
      maybeCached match
        case Some((_, t)) => Some(t)
        case None    => r.geckoTokens.find(_.contract.map(_ == contract).getOrElse(false))

    def resolveToken(token: Token): Option[Token] = 
      findById(token.id)
        .orElse(token.contract.flatMap(findByContract))
        .orElse {
          val grp = r.geckoTokens.filter(_.symbol == token.symbol)
          val bestMatch = scoreByName(token.name, grp).maxBy(_._2)._1
          Some(bestMatch)
        }

    def bluechipFor(chain: Chain): Try[Token] = 
      if r.persistentCache.values.exists(_.symbol == chain.symbol)
        then cachedBluechipBySymbol(chain.symbol, r)
        else geckoBluechipBySymbol(chain.symbol, r)

    def saveCache(cacheFile: FilePath): IO[Unit] =
      saveJsonFile(cacheFile, r.persistentCache.map(_._2).toSeq)

  //////////////////////
  // Object Functions //
  //////////////////////

  def apply(cacheFile: FilePath, geckoList: Seq[Token]): IO[TokenRegistry] =
    val cache = if cacheFile.exists
      then loadJsonFile[Seq[Token]](cacheFile).map(_.map(v => v.id -> v).toMap)
      else IO.pure(Map.empty[Id, Token])

    cache.map(TokenRegistry(geckoList, Map.empty, _))

  private def scoreByName(name: String, group: Seq[Token]): Seq[(Token, Double)] =
    group.map(t => t -> Cosine.score(t.name, name))

  private def cachedBluechipBySymbol(s: Symbol, r: TokenRegistry): Try[Token] = 
    r.persistentCache.values.find(_.symbol == s) match
      case Some(t) => Success(t)
      case None    => Failure(new Exception(s"Cached bluechip reported but not found: ${s.show}"))

  private def geckoBluechipBySymbol(s: Symbol, r: TokenRegistry): Try[Token] = 
    val gid = s match
      case Symbol.Btc   => "bitcoin"
      case Symbol.Eth   => "ethereum"
      case Symbol.Sol   => "solana"
      case Symbol.Egld  => "elrond"
      case Symbol.Bnb   => "binance-smart-chain"
      case Symbol.Avax  => "avalanche"
      case Symbol.Ftm   => "fantom"
      case Symbol.Matic => "polygon"
      case Symbol.One   => "harmony"
      case Symbol.Doge  => "dogecoin"
      case Symbol.Dot   => "polkadot"
      case Symbol.Ada   => "cardano"

    r.geckoTokens.find(_.geckoId == gid) match
      case Some(t) => Success(t)
      case None    => Failure(new Exception(s"Unable to find bluechip on coingecko: ${s.show} with geckoid=$gid"))
