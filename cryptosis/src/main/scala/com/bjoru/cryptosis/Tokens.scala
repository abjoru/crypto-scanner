package com.bjoru.cryptosis

import cats.effect.IO
import cats.syntax.foldable.given

import org.http4s.Uri
import org.http4s.client.*
import org.http4s.circe.CirceEntityDecoder.given

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*
import com.bjoru.cryptosis.config.GToken
import com.bjoru.cryptosis.utils.CoingeckoMapper

import scala.util.{Try, Success, Failure}

final case class Tokens(
  cacheDirectory: FilePath,
  cachedTokens: Map[Id, Token],
  coingeckoTokens: Seq[Token]
)

object Tokens:

  val CACHE_TOKEN_FILE = "cryptosis-tokens.json"
  val GECKO_TOKEN_FILE = "coingecko-tokens.json"

  extension (t: Tokens)

    def findById(id: Id): Try[Token] = Try(t.cachedTokens(id))

    def findBySymbol(symbol: Symbol, chain: Chain = Chain.Unknown): Try[Token] =
      val bySymbol = t.cachedTokens.values.filter(_.symbol == symbol)
      val groups = if chain != Chain.Unknown
                     then bySymbol.filter(_.chain == chain)
                     else bySymbol

      groups.toList match
        case h :: Nil => Success(h)
        case other    => Failure(Exception(s"Too many candidates for $symbol: ${other.mkString(",")}"))

    def findByContract(contract: Address): Try[Token] = 
      t.cachedTokens.find(_._2.contract.map(_ == contract).getOrElse(false)) match
        case Some((_, t)) => Success(t)
        case None         => t.coingeckoTokens
                              .find(_.contract.map(_ == contract).getOrElse(false))
                              .fold(Failure(Exception(s"No token with contract: $contract")))(Success(_))

    def resolveToken(token: Token): Try[(Tokens, Token)] = 
      internalResolveToken(token, t).map { newToken =>
        t.copy(cachedTokens = t.cachedTokens.updated(newToken.id, newToken)) -> newToken
      }

    def resolveTokens(tokens: Seq[Token]): Try[(Tokens, Seq[Token])] =
      tokens.foldLeftM(t -> Seq.empty[Token]) {
        case ((t2, acc), x) =>
          val result = t2.resolveToken(x)
          result.map(v => v._1 -> (acc :+ v._2))
      }

    def save: IO[Unit] = saveJsonFile(t.cacheDirectory </> CACHE_TOKEN_FILE, t.cachedTokens.values.toSeq)

  def loadTokens(cacheDir: FilePath)(using Client[IO]): IO[Tokens] =
    for cached <- cachedTokens(cacheDir)
        geckos <- coingeckoTokens(cacheDir)
    yield Tokens(cacheDir, cached.map(v => v.id -> v).toMap, geckos)

  private def cachedTokens(cacheDir: FilePath): IO[Seq[Token]] =
    if (cacheDir </> CACHE_TOKEN_FILE).exists
      then loadJsonFile[Seq[Token]](cacheDir </> CACHE_TOKEN_FILE)
      else IO.pure(Seq.empty[Token])

  private def coingeckoTokens(cacheDir: FilePath)(using Client[IO]): IO[Seq[Token]] =
    if (cacheDir </> GECKO_TOKEN_FILE).expired(30)
      then fetchCoingeckoTokens(cacheDir)
      else loadJsonFile[Seq[Token]](cacheDir </> GECKO_TOKEN_FILE)

  private def fetchCoingeckoTokens(cacheDir: FilePath)(using client: Client[IO]): IO[Seq[Token]] =
    for gt <- IO.fromEither(Uri.fromString("https://api.coingecko.com/api/v3/coins/list?include_platform=true"))
        dx <- client.expect[Seq[GToken]](gt)
        rx  = CoingeckoMapper.processGeckos(dx)
        _   = (cacheDir </> GECKO_TOKEN_FILE).delete
        _  <- saveJsonFile(cacheDir </> GECKO_TOKEN_FILE, rx)
    yield rx

  private def internalResolveToken(token: Token, api: Tokens): Try[Token] = 
    val result = api.findById(token.id)
      .orElse(token.contract.map(api.findByContract).getOrElse(Failure(Exception(s"No contract defined for $token"))))
      .orElse {
        api.coingeckoTokens.filter(g => g.symbol == token.symbol && g.chain == token.chain) match
          case xs if xs.isEmpty => Failure(Exception(s"No such token: $token"))
          case Seq(h)           => Success(h)
          case grp              => Success(CoingeckoMapper.bestMatched(token, grp))
      }

    // map variable fields back to resolved token
    result.map(_.base(token))
