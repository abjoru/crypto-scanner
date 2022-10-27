package com.bjoru.cryptosis

import cats.effect.IO
import cats.syntax.foldable.given

import org.http4s.client.Client

import com.bjoru.cryptosis.types.*
import com.bjoru.cryptosis.oracles.*

class CryptoEnv(val priceApi: PricingApi, tokenApi: TokenApi) extends Env:

  def register(items: (Token | Defi)*)(using Client[IO]): IO[Result[Seq[Token | Defi]]] =
    val newPriceApi = items.foldLeft(priceApi) {
      case (acc, t: Token) => acc.register(t)
      case (acc, d: Defi)  => acc.register(d.tokens: _*)
    }

    val resolved = items.foldLeftM(Seq.empty[Token | Defi]) {
      case (acc, t: Token) => 
        tokenApi.resolve(t).map(acc ++ _)
      case (acc, d: Defi.Stake) => 
        tokenApi.resolve(d.liquidity: _*).map(v => acc :+ d.copy(liquidity = v))
      case (acc, d: Defi.Farm) =>
        for a <- tokenApi.resolve(d.liquidity: _*)
            b <- tokenApi.resolve(d.claimable: _*)
        yield acc :+ d.copy(liquidity = a, claimable = b)
      case (acc, d: Defi.Pool) =>
        for a <- tokenApi.resolve(d.liquidity: _*)
            b <- tokenApi.resolve(d.poolToken)
        yield acc :+ d.copy(liquidity = a, poolToken = b.head)
    }

    resolved.map(v => Result.of(v)(using CryptoEnv(newPriceApi, tokenApi)))

  def bluechip(chain: Chain)(using Client[IO]): IO[Token] = 
    tokenApi.bluechip(chain)

  def token(symbol: Symbol, chain: Chain)(using Client[IO]): IO[Option[Token]] = 
    tokenApi.lookup(symbol, chain)

  def syncPrices(using Client[IO]): IO[Env] = priceApi.syncPrices.map(CryptoEnv(_, tokenApi))

object CryptoEnv: 

  def coingecko: Env = CryptoEnv(CoingeckoPricing(Map.empty), new CoingeckoTokens)
