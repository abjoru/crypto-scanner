package com.bjoru.cryptosis.types

import cats.data.StateT
import cats.effect.IO
import cats.implicits.*

import org.http4s.client.Client

import pureconfig.*

import com.bjoru.cryptosis.*

import scala.util.Try

final case class Wallet(
  name:     String,
  chain:    Chain,
  address:  Address,
  balances: Map[Id, Wallet.Item]
)

object Wallet:

  type Item = Token | Defi

  given ConfigReader[Wallet] = ConfigReader.forProduct3("name", "chain", "address")(
    (name: String, chain: Chain, address: Address) => Wallet(name, chain, address, Map.empty)
  )

  extension (w: Wallet)

    def isMultichain: Boolean = w.chain == Chain.Ethereum

    def withChain(chain: Chain): Wallet =
      w.copy(chain = chain)
    
    def addBalance(item: Item): Wallet = item match
      case t: Token => w.copy(balances = w.balances.updated(t.id, t))
      case d: Defi  => w.copy(balances = w.balances.updated(d.id, d))

    def addBalances(items: Seq[Item]): Wallet = items.foldLeft(w) {
      case (acc, t: Token) => acc.copy(balances = acc.balances.updated(t.id, t))
      case (acc, d: Defi)  => acc.copy(balances = acc.balances.updated(d.id, d))
    }

    def unpricedTokens: Set[Token] = w.balances.foldLeft(Set.empty[Token]) {
      case (acc, (_, t: Token)) if t.missingPrice => acc + t
      case (acc, (_, d: Defi))                    => acc ++ d.unpricedTokens
      case (acc, _)                               => acc
    }

    def valueUsd: Try[Price] = 
      val values = w.balances.toSeq.traverse {
        case (_, t: Token) => t.valueUsd
        case (_, d: Defi)  => d.valueUsd
      }

      values.map {
        case xs if xs.isEmpty => Price.Zero
        case xs               => xs.reduce(_ + _)
      }

    def priceTokens(using Client[IO]): IO[Wallet] = 
      val newB = w.balances.toList.foldLeftM(Map.empty[Id, Item]) {
        case (acc, (i, t: Token)) => 
          Env.priced(t).map(v => acc + (i -> v.head))
        case (acc, (i, d: Defi.Stake)) =>
          Env.priced(d.liquidity: _*)
            .map(v => acc + (i -> d.copy(liquidity = v)))
        case (acc, (i, d: Defi.Farm)) =>
          for a <- Env.priced(d.liquidity: _*)
              b <- Env.priced(d.claimable: _*)
          yield acc + (i -> d.copy(liquidity = a, claimable = b))
        case (acc, (i, d: Defi.Pool)) =>
          Env.priced(d.liquidity: _*)
            .map(v => acc + (i -> d.copy(liquidity = v)))
      }

      newB.map(b => w.copy(balances = b))

    def merge(other: Wallet): Wallet = 
      w.addBalances(other.balances.values.toSeq)

    def balancesNoDust: Map[Id, Item] = w.balances.filterNot {
      case (_, t: Token) if t.isEmpty => true
      case _                          => false
    }

  def loadWallets(path: FilePath): IO[Seq[Wallet]] = 
    loadYamlFile[Seq[Wallet]](path)

  def mergeWallets(a: Seq[Wallet], b: Seq[Wallet]): Seq[Wallet] =
    val bMap = b.map(v => v.address -> v).toMap

    a.map(v => bMap.get(v.address).map(v.merge(_)).getOrElse(v))
