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

class Env(val registry: Map[Id, Token], val priceRegistry: Map[Id, Price]):

  def price(token: Token): IO[(Env, Price)] = ???

  def prices(tokens: Seq[Token]): IO[(Env, Map[Id, Price])] = ???

  def setPrice(token: Token, price: Price): Env = 
    if registry.contains(token.id)
      then Env(registry, priceRegistry.updated(token.id, price))
      else Env(registry.updated(token.id, token), priceRegistry.updated(token.id, price))

  def setPrices(prices: Seq[(Token, Price)]): Env =
    prices.foldLeft(this) { case (acc, (t, p)) => acc.setPrice(t, p) }

  // FIXME clean this up!
  // reintroduce generic Id based on non-gecko fields and use
  // string matching algo on name as last resort when multiple candidates
  def findToken(symbol: Symbol, chain: Chain, contract: Option[Address]): Option[Token] =
    registry.values.toList.filter(_.symbol == symbol) match
      case Nil if contract.isDefined => 
        println("not found by symbol!")
        registry.values.find { t =>
          val res = for a <- t.contract
                        b <- contract
                    yield a == b

          res.getOrElse(false)
        }

      case Nil =>
        println("still not found by symbol!")
        None

      case h :: Nil => Some(h)

      case group if chain == Chain.Unknown && contract.isDefined =>
        println(s"searching in ${group.mkString(",")}")
        group.find { t =>
          val res = for a <- t.contract
                        b <- contract
                    yield a == b

          res.getOrElse(false)
        }

      case group if chain == Chain.Unknown =>
        group.filterNot(_.name.toLowerCase.contains("wrapped")).headOption

      case group => group.find(_.chain == chain)

  def saveRegistry(file: FilePath): IO[Unit] = 
    for r <- IO(file.toFile.delete())
        u <- saveJsonFile(file, registry.values.toSeq)
    yield u

object Env:

  def loadRegistry(file: FilePath)(client: Client[IO]): IO[Env] = 
    if file.toFile.exists
      then loadJsonFile[Seq[Token]](file).map(buildRegistry)
      else fetchRegistry(file, client)

  private def buildRegistry(tokens: Seq[Token]): Env = 
    val reg = tokens.map(t => t.id -> t).toMap
    Env(reg, Map.empty)

  private def fetchRegistry(file: FilePath, client: Client[IO]): IO[Env] =
    for gt <- IO.fromEither(Uri.fromString("https://api.coingecko.com/api/v3/coins/list?include_platform=true"))
        dx <- client.expect[Seq[GToken]](gt)
        tx  = processGeckos(dx)
        _  <- saveJsonFile(file, tx)
    yield Env(tx.map(t => t.id -> t).toMap, Map.empty)

  private def processGeckos(gtokens: Seq[GToken]): Seq[Token] = 
    val listOfLists = gtokens.map { 
      case GToken(id@"bitcoin", sym@Symbol.Btc, name, _) =>
        Seq(Token(id, name, sym, Chain.Bitcoin, 8, None))
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
