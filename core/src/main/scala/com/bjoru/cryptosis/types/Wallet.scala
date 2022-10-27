package com.bjoru.cryptosis.types

import cats.effect.IO
import cats.implicits.*

import pureconfig.*

import com.bjoru.cryptosis.*

import scala.util.Try

final case class Wallet(
  name:     String,
  chain:    Chain,
  address:  Address,
  balances: Map[Id, Token | Defi]
)

object Wallet:

  given ConfigReader[Wallet] = ConfigReader.forProduct3("name", "chain", "address")(
    (name: String, chain: Chain, address: Address) => Wallet(name, chain, address, Map.empty)
  )

  extension (w: Wallet)

    def addBalances(items: (Token | Defi)*): Wallet = items.foldLeft(w) {
      case (acc, t: Token) => acc.copy(balances = acc.balances.updated(t.id, t))
      case (acc, d: Defi)  => acc.copy(balances = acc.balances.updated(d.id, d))
    }

    def isMultichain: Boolean = w.chain == Chain.Ethereum

    def withChain(chain: Chain): Wallet = w.copy(chain = chain)

    def merge(other: Wallet): Wallet =
      w.addBalances(other.balances.values.toSeq: _*)

  def mergeAll(a: Seq[Wallet], b: Seq[Wallet]): Seq[Wallet] =
    val map = b.map(v => v.address -> v).toMap
    a.map(v => map.get(v.address).map(v.merge(_)).getOrElse(v))

  def loadWallets(file: FilePath): IO[Seq[Wallet]] =
    loadYaml[Seq[Wallet]](file)
