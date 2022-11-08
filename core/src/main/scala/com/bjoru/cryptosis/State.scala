package com.bjoru.cryptosis

import cats.effect.IO

import io.circe.*
import io.circe.syntax.*

import org.http4s.client.Client

import com.bjoru.cryptosis.types.*

import scala.concurrent.duration.*

final case class State private(
  tokens: Map[Id, Token],
  oracleTokens: Map[Id, Token],
  unknownTokens: Map[Id, Token],
  prices: Map[Id, Price],
  fTokens: FilePath,
  fUnknowns: FilePath,
  fOracle:   FilePath,
  oracle:    Oracle
)

object State:

  given Encoder[Token] = Encoder.instance { token =>
    Json.obj(
      "id"       -> token.geckoId.asJson,
      "name"     -> token.name.asJson,
      "symbol"   -> token.symbol.asJson,
      "chain"    -> token.chain.asJson,
      "decimals" -> token.decimals.asJson,
      "contract" -> token.contract.asJson
    )
  }

  given Decoder[Token] = Decoder.instance { hc =>
    for i <- hc.downField("id").as[String]
        n <- hc.downField("name").as[String]
        s <- hc.downField("symbol").as[Symbol]
        c <- hc.downField("chain").as[Chain]
        d <- hc.downField("decimals").as[Int]
        a <- hc.downField("contract").as[Option[Address]]
    yield Token(i, n, s, c, a, d, Balance.Zero)
  }

  extension (s: State)

    def resolve(token: Token): (State, Token) = 
      s.tokens.get(token.id) match
        case Some(t) => s -> t.basedOn(token)
        case None    => resolveFromOracle(token, s)

    def resolveAll(tokens: Seq[Token]): (State, Seq[Token]) =
      tokens.foldLeft(s -> Seq.empty[Token]) {
        case ((s2, tx), t) => s2.resolve(t) match
          case (s3, t2) => s3 -> (tx :+ t2)
      }

    def resolveAllWithPrice(tokens: Seq[(Token, Price)]): (State, Seq[Token]) =
      tokens.foldLeft(s -> Seq.empty[Token]) {
        case ((s2, tx), (t, p)) => 
          val (s3, t2) = s2.resolve(t)
          val (s4, t3) = s3.registerPrice(t2, p)
          s4 -> (tx :+ t3)
      }

    def resolveApp(dapp: Defi): (State, Defi) = dapp match
      case a: Defi.Stake =>
        val (s2, ts) = s.resolveAll(a.liquidity)
        s2 -> a.copy(liquidity = ts)
      case a: Defi.Farm =>
        val (s2, lq) = s.resolveAll(a.liquidity)
        val (s3, cl) = s2.resolveAll(a.claimable)
        s3 -> a.copy(liquidity = lq, claimable = cl)
      case a: Defi.Pool =>
        val (s2, lq) = s.resolveAll(a.liquidity)
        val (s3, pt) = s2.resolve(a.poolToken)
        s3 -> a.copy(liquidity = lq, poolToken = pt)

    def resolveAllApps(dapps: Seq[Defi]): (State, Seq[Defi]) =
      dapps.foldLeft(s -> Seq.empty[Defi]) {
        case ((s2, ds), d) => s2.resolveApp(d) match
          case (s3, d2) => s3 -> (ds :+ d2)
      }

    def priceOf(token: Token): Price = 
      s.prices.get(token.id).getOrElse(Price.Zero)

    def valueOf(token: Token): Price =
      s.priceOf(token) * token.balance

    def valueOf(app: Defi): Price = app match 
      case Defi.Stake(_, _, _, l)   => l.map(s.valueOf).sum
      case Defi.Farm(_, _, _, l, c) => l.map(s.valueOf).sum + c.map(s.valueOf).sum
      case Defi.Pool(_, _, _, l, p) => l.map(s.valueOf).sum + valueOf(p)

    def valueOf(wallet: Wallet): Price =
      val pl = wallet.balances.toSeq.map {
        case (_, t: Token) => s.valueOf(t)
        case (_, d: Defi)  => s.valueOf(d)
      }

      pl.sum

    def valueOf(exhange: Exchange): Price =
      val pl = exhange.balances.toSeq.map {
        case (_, t: Token) => s.valueOf(t)
        case (_, d: Defi)  => s.valueOf(d)
      }

      pl.sum

    def bluechip(chain: Chain): (State, Token) = 
      val id = Token.mkId(chain.symbol, chain)
      s.tokens.get(id) match
        case Some(t) => s -> t
        case None =>
          val bc = s.oracleTokens(id)
          updateTokens(Seq(bc)) -> bc

    def exchangeToken(tok: ExchangeToken): (State, Token) =
      s.tokens.get(tok.id) match
        case Some(t) => s -> t.withBalance(tok.balance)
        case None =>
          val bc = s.oracleTokens(tok.id)
          updateTokens(Seq(bc)) -> bc.withBalance(tok.balance)

    def exchangeTokens(pairs: Seq[ExchangeToken]): (State, Seq[Token]) =
      pairs.foldLeft(s -> Seq.empty[Token]) {
        case ((s2, acc), tok) => 
          val (s3, to) = s2.exchangeToken(tok)
          s3 -> (acc :+ to.withBalance(tok.balance))
      }

    def syncPrices(using Client[IO]): IO[State] = 
      s.oracle.fetchPrices(s.tokens.values.toSeq).map { result =>
        s.copy(prices = s.prices ++ result)
      }

    def registerPrice(token: Token, price: Price): (State, Token) =
      s.copy(prices = s.prices + (token.id -> price)) -> token

    def updateTokens(tx: Seq[Token]): State =
      s.copy(tokens = s.tokens ++ tx.map(t => t.id -> t).toMap)

    def updateUnknowns(tx: Seq[Token]): State =
      s.copy(unknownTokens = s.unknownTokens ++ tx.map(t => t.id -> t).toMap)

    def saveAll: IO[Unit] = 
      for _ <- saveJson(s.fTokens, s.tokens.values.toSeq)
          _ <- saveJson(s.fUnknowns, s.unknownTokens.values.toSeq)
      yield ()

  def apply(
    tokensFile: FilePath,
    unknownTokensFile: FilePath,
    cachedOracleTokensFile: FilePath,
    oracle: Oracle
  )(using Client[IO]): IO[State] = 
    for a <- loadJsonOrElse[Seq[Token]](tokensFile)(Seq.empty)
        b <- loadOrFetchTokens(cachedOracleTokensFile, oracle)
    yield State(
      tokens = a.map(t => t.id -> t).toMap, 
      oracleTokens = b, 
      unknownTokens = Map.empty, 
      prices = Map.empty,
      fTokens = tokensFile,
      fUnknowns = unknownTokensFile,
      fOracle = cachedOracleTokensFile,
      oracle = oracle
    )

  private def loadOrFetchTokens(
    file: FilePath, 
    oracle: Oracle
  )(using Client[IO]): IO[Map[Id, Token]] =
    if file.expired(30.days)
      then oracle.fetchTokens(file)
      else loadJson[Seq[Token]](file).map(_.map(t => t.id -> t).toMap)

  private def resolveFromOracle(token: Token, state: State): (State, Token) =
    state.oracleTokens.get(token.id) match
      case Some(t) => 
        val tout = t.basedOn(token)
        state.updateTokens(Seq(tout)) -> tout
      case None => 
        state.updateUnknowns(Seq(token)) -> token
