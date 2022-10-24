package com.bjoru.cryptosis

import cats.effect.IO
import cats.syntax.show.given

import com.bjoru.cryptosis.types.*

import com.github.vickumar1981.stringdistance.StringDistance.*
import com.github.vickumar1981.stringdistance.given

import scala.util.{Try, Success, Failure}

final case class TokenRegistry(
  geckoTokens: Seq[Token],
  geckoPrices: Map[Id, Price],
  persistentCache: Map[Id, Token]
)

object TokenRegistry:

  extension (r: TokenRegistry)

    def updateToken(token: Token): TokenRegistry =
      r.copy(persistentCache = r.persistentCache.updated(token.id, token))

    def updatePrice(id: Id, price: Price): TokenRegistry =
      r.copy(geckoPrices = r.geckoPrices.updated(id, price))

    def updatePrices(prices: Seq[(Id, Price)]): TokenRegistry =
      prices.foldLeft(r) { case (acc, (i, p)) => acc.updatePrice(i, p) }

    def findById(id: Id): Option[Token] = r.persistentCache.get(id)

    def findByContract(contract: Address): Option[Token] =
      val maybeCached = r.persistentCache.find(_._2.contract.map(_ == contract).getOrElse(false))
      maybeCached match
        case Some((_, t)) => Some(t)
        case None    => r.geckoTokens.find(_.contract.map(_ == contract).getOrElse(false))

    def resolveToken(token: Token): Option[Token] = 
      val maybeToken = findById(token.id)
        .orElse(token.contract.flatMap(findByContract))
        .orElse {
          // FIXME score multiple fields for better results
          r.geckoTokens.filter(g => g.symbol == token.symbol && g.chain == token.chain) match
            case xs if xs.isEmpty => None
            case Seq(h)           => Some(h)
            case grp              => Some(bestMatched(token, grp))
            //scoreByName(token.name, grp).maxBy(_._2)._1)
        }

      maybeToken.map(_.base(token))

    def resolveAndUpdate(token: Token): (TokenRegistry, Token) =
      resolveToken(token) match
        case Some(t) => r.updateToken(t) -> t
        case None    => r.updateToken(token) -> token

    def resolveAndUpdateAll(tokens: Seq[Token]): (TokenRegistry, Seq[Token]) =
      tokens.foldLeft(r -> Seq.empty[Token]) {
        case ((r2, acc), t) => 
          val (r3, t2) = resolveAndUpdate(t)
          r3 -> (acc :+ t2)
      }

    def bluechipFor(chain: Chain): Try[Token] = 
      if r.persistentCache.values.exists(t => t.symbol == chain.symbol && t.chain == chain)
        then cachedBluechipBySymbol(chain.symbol, r)
        else geckoBluechipBySymbol(chain.symbol, r)

    def priceOf(token: Token): Token = 
      if !token.missingPrice
        then token
        else r.geckoPrices.get(token.id) match
               case Some(p) => token.withPrice(p)
               case None    => token.withPrice(Price.Zero)

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

  private def score(value: String, group: Seq[Token])(f: Token => String): Seq[(Token, Double)] =
    group.map(t => t -> Cosine.score(f(t), value))

  private def bestMatched(target: Token, group: Seq[Token]): Token =
    val idScores = score(target.geckoId, group)(_.geckoId)
    val nameScoresMap = score(target.name, group)(_.name).toMap

    val scores = idScores.foldLeft(Seq.empty[(Token, Double)]) {
      case (acc, r@(t, s)) => nameScoresMap.get(t) match
        case Some(score) => acc :+ (t -> (s + score))
        case None        => acc :+ r
    }

    scores.maxBy(_._2)._1

  private def cachedBluechipBySymbol(s: Symbol, r: TokenRegistry): Try[Token] = 
    r.persistentCache.values.find(_.symbol == s) match
      case Some(t) => Success(t)
      case None    => Failure(new Exception(s"Cached bluechip reported but not found: ${s.show}"))

  private def geckoBluechipBySymbol(s: Symbol, r: TokenRegistry): Try[Token] = 
    val gid = s match
      case Symbol.Btc   => "bitcoin"
      case Symbol.Eth   => "ethereum"
      case Symbol.Sol   => "solana"
      case Symbol.Egld  => "elrond-erd-2"
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

