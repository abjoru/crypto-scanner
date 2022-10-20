package com.bjoru.cryptosis.types

import cats.data.StateT
import cats.effect.IO
import cats.implicits.*

import pureconfig.*

import com.bjoru.cryptosis.*

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

  def loadWallets(path: FilePath): IO[Seq[Wallet]] = 
    loadYamlFile[Seq[Wallet]](path)
