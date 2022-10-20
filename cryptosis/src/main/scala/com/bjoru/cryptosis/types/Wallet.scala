package com.bjoru.cryptosis.types

import cats.data.StateT
import cats.effect.IO
import cats.implicits.*

import pureconfig.*

import com.bjoru.cryptosis.*

import scala.util.Try

final case class Wallet(
  name:     String,
  chain:    Chain,
  address:  Address,
  balances: Map[Id, Token | DApp]
)

object Wallet:

  given ConfigReader[Wallet] = ConfigReader.forProduct3("name", "chain", "address")(
    (name: String, chain: Chain, address: Address) => Wallet(name, chain, address, Map.empty)
  )

  extension (w: Wallet)
    
    def addBalance(item: Token | DApp): Wallet = item match
      case t: Token => w.copy(balances = w.balances.updated(t.id, t))
      case d: DApp  => w.copy(balances = w.balances.updated(d.id, d))

    def addBalances(items: Seq[Token | DApp]): Wallet = items.foldLeft(w) {
      case (acc, t: Token) => acc.copy(balances = acc.balances.updated(t.id, t))
      case (acc, d: DApp)  => acc.copy(balances = acc.balances.updated(d.id, d))
    }

    def unpricedTokens: Set[Token] = w.balances.foldLeft(Set.empty[Token]) {
      case (acc, (_, t: Token)) if t.missingPrice => acc + t
      case (acc, (_, d: DApp))                    => acc ++ d.unpricedTokens
      case (acc, _)                               => acc
    }

    def valueUsd: Try[Price] = 
      val values = w.balances.toSeq.traverse {
        case (_, t: Token) => t.valueUsd
        case (_, d: DApp)  => d.valueUsd
      }

      values.map(_.reduce(_ + _))

  def loadWallets(path: FilePath): IO[Seq[Wallet]] = 
    loadYamlFile[Seq[Wallet]](path)
