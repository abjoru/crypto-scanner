package com.bjoru.cryptosis.utils

import com.bjoru.cryptosis.types.*
import com.bjoru.cryptosis.config.GToken

import com.github.vickumar1981.stringdistance.given
import com.github.vickumar1981.stringdistance.StringDistance.Cosine

object CoingeckoMapper:

  val bluechipSymbolTable = Map(
    Symbol.Btc   -> "bitcoin",
    Symbol.Eth   -> "ethereum",
    Symbol.Sol   -> "solana",
    Symbol.Egld  -> "elrond-erd-2",
    Symbol.Bnb   -> "binance-smart-chain",
    Symbol.Avax  -> "avalanche",
    Symbol.Ftm   -> "fantom",
    Symbol.Matic -> "polygon",
    Symbol.One   -> "harmony",
    Symbol.Doge  -> "dogecoin",
    Symbol.Dot   -> "polkadot",
    Symbol.Ada   -> "cardano"
  )

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

  def processGeckos(tokens: Seq[GToken]): Seq[Token] = 
    val tokenList = tokens.map {
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

    tokenList.flatten.filterNot(_.chain == Chain.Unknown)

  def resolveChain(geckoId: String): Chain =
    geckoId.split('-').foldLeft(Chain.Unknown) {
      case (acc, str) if acc != Chain.Unknown => acc
      case (_, str) => chainIdMap.get(str).getOrElse(Chain.Unknown)
    }

  def bestMatched(token: Token, group: Seq[Token]): Token =
    val iScores = score(token.geckoId, group)(_.geckoId)
    val nScores = score(token.name, group)(_.name).toMap

    val scores = iScores.foldLeft(Seq.empty[(Token, Double)]) {
      case (acc, r@(t, s)) => nScores.get(t) match
        case Some(score) => acc :+ (t -> (s + score))
        case None        => acc :+ r
    }

    scores.maxBy(_._2)._1

  private def score(value: String, group: Seq[Token])(f: Token => String): Seq[(Token, Double)] =
    group.map(t => t -> Cosine.score(f(t), value))
