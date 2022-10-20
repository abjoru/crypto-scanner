package com.bjoru.cryptosis.types

import cats.data.StateT
import cats.effect.IO
import cats.implicits.*

import pureconfig.*

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.config.*

final case class Wallet(
  name: String,
  chain: Chain,
  address: Address,
  balances: Map[Id, Wallet.Item]
)

object Wallet:

  type Item = (Token, Balance) | DApp

  given ConfigReader[Wallet] = ConfigReader.forProduct3("name", "chain", "address")(
    (name: String, chain: Chain, address: Address) => Wallet(name, chain, address, Map.empty)
  )

  extension (w: Wallet)
    
    // FIXME refresh price data if we get some from provider!
    def addBalance(item: Item): StateT[IO, Env, Wallet] = StateT.pure {
      item match
        case (t: Token, b: Balance) => 
          w.copy(balances = w.balances.updated(t.id, (t, b)))
        case d: DApp =>
          w.copy(balances = w.balances.updated(d.id, d))
    }

    // FIXME refresh price data if we get some from provider!
    def addBalances(items: Seq[Item]): StateT[IO, Env, Wallet] = StateT.pure {
      items.foldLeft(w) {
        case (acc, (t: Token, b: Balance)) =>
          acc.copy(balances = acc.balances.updated(t.id, (t, b)))
        case (acc, d: DApp) =>
          acc.copy(balances = acc.balances.updated(d.id, d))
      }
    }

    def tokens = w.balances.collect {
      case (_, (t: Token, _)) => t
    }

    def dapps = w.balances.collect {
      case (_, d: DApp) => d
    }

    def tokenValue: StateT[IO, Env, Price] = StateT { env =>
      val ioPrice = env.prices(w.tokens.toSeq).map {
        case (env2, priceMap) =>
          w.balances.foldLeft(Price.Zero) {
            case (acc, (id, (_, b: Balance))) =>
              acc + priceMap.get(id).map(_ * b.toBigDecimal).getOrElse(Price.Zero)
            case (acc, _) => acc
          }
      }

      ioPrice.map(env -> _)
    }
      /*
      for tokens <- StateT.pure(w.tokens)
          prices <- StateT.inspectF[IO, Env, (Token, Price)](_.prices(tokens.toSeq))
          totals  = prices.foldLeft(Price.Zero) {
                      case (acc, (t, p)) => w.balances.get(t.id) match
                        case (_, b: Balance) => acc + (b * p)
                    }
      yield totals
      */

  def loadWallets(path: FilePath): IO[Seq[Wallet]] = 
    loadYamlFile[Seq[Wallet]](path)
