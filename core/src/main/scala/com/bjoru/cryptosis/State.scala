package com.bjoru.cryptosis

import cats.effect.IO
import cats.syntax.traverse.*
import cats.syntax.foldable.*

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

    def saveAll: IO[Unit] = 
      for _ <- saveJson(s.fTokens, s.tokens.values.toSeq)
          _ <- saveJson(s.fUnknowns, s.unknownTokens.values.toSeq)
      yield ()

  ///////////////////
  // State Actions //
  ///////////////////

  def resolve(token: Token): SIO[Token] = 
    SIO.inspect(_.tokens.get(token.id)).flatMap {
      case Some(t) => SIO.pure(t.basedOn(token))
      case None    => resolveFromOracle(token)
    }

  def resolveAll(tokens: Seq[Token]): SIO[Seq[Token]] =
    tokens.traverse(resolve) // FIXME not sure this properly threads state!

  def resolveAllWithPrice(tokens: Seq[(Token, Price)]): SIO[Seq[Token]] =
    tokens.traverse {
      case (token, price) =>
        for a <- resolve(token)
            b <- registerPrice(a, price)
        yield b
    }

  def resolveApp(dapp: Defi): SIO[Defi] = dapp match
    case a: Defi.Stake =>
      resolveAll(a.liquidity).map(v => a.copy(liquidity = v))
    case a: Defi.Farm =>
      for x <- resolveAll(a.liquidity)
          y <- resolveAll(a.claimable)
      yield a.copy(liquidity = x, claimable = y)
    case a: Defi.Pool =>
      for x <- resolve(a.poolToken)
          y <- resolveAll(a.liquidity)
      yield a.copy(poolToken = x, liquidity = y)

  def resolveAllApps(dapps: Seq[Defi]): SIO[Seq[Defi]] =
    dapps.traverse(resolveApp)

  def resolveExchangeToken(token: ExchangeToken): SIO[Token] =
    SIO.inspect(_.tokens.get(token.id)).flatMap {
      case Some(t) => SIO.pure(t.withBalance(token.balance))
      case None =>
        for a <- SIO.inspect(_.oracleTokens(token.id))
            b <- updateToken(a)
        yield b
    }

  def resolveExchangeTokens(tokens: Seq[ExchangeToken]): SIO[Seq[Token]] =
    tokens.traverse(resolveExchangeToken)

  private def resolveFromOracle(token: Token): SIO[Token] = 
    SIO.inspect(_.oracleTokens.get(token.id)).flatMap {
      case Some(t) => updateToken(t.basedOn(token))
      case None    => updateUnknown(token)
    }

  def registerPrice(token: Token, price: Price): SIO[Token] = SIO { s =>
    IO.pure(s.copy(prices = s.prices + (token.id -> price)) -> token)
  }

  def bluechip(chain: Chain): SIO[Token] = 
    val id = Token.mkId(chain.symbol, chain)
    SIO.inspect(_.tokens.get(id)).flatMap {
      case Some(t) => SIO.pure(t)
      case None    => 
        for a <- SIO.inspect(_.oracleTokens(id))
            b <- updateToken(a)
        yield b
    }

  def updateToken(token: Token): SIO[Token] = SIO { s =>
    IO.pure(s.copy(tokens = s.tokens.updated(token.id, token)) -> token)
  }

  def updateTokens(tokens: Seq[Token]): SIO[Seq[Token]] = SIO { s =>
    IO.pure(s.copy(tokens = s.tokens ++ tokens.map(t => t.id -> t).toMap) -> tokens)
  }

  def updateUnknown(token: Token): SIO[Token] = SIO { s =>
    IO.pure(s.copy(unknownTokens = s.unknownTokens.updated(token.id, token)) -> token)
  }

  def updateUnknowns(tokens: Seq[Token]): SIO[Seq[Token]] = SIO { s =>
    IO.pure(s.copy(unknownTokens = s.unknownTokens ++ tokens.map(t => t.id -> t).toMap) -> tokens)
  }

  def syncPrices(using Client[IO]): SIO[Unit] = SIO.modifyF { state =>
    state.oracle.fetchPrices(state.tokens.values.toSeq).map { result =>
      state.copy(prices = state.prices ++ result)
    }
  }

  def save: SIO[Unit] = SIO.inspectF(_.saveAll)


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


  //private def resolveFromOracle(token: Token, state: State): (State, Token) =
    //state.oracleTokens.get(token.id) match
      //case Some(t) => 
        //val tout = t.basedOn(token)
        //state.updateTokens(Seq(tout)) -> tout
      //case None => 
        //state.updateUnknowns(Seq(token)) -> token
