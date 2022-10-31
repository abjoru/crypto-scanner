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
  balances: Map[Id, Token | Defi],
  filters:  Map[Chain, TokenFilter]
)

object Wallet:

  given ConfigReader[Wallet] = ConfigReader.forProduct3("name", "chain", "address")(
    (name: String, chain: Chain, address: Address) => Wallet(name, chain, address, Map.empty, Map.empty)
  )

  extension (w: Wallet)

    def addBalances(items: (Token | Defi)*): Wallet = items.foldLeft(w) {
      case (acc, t: Token) if accept(t) => acc.copy(balances = acc.balances.updated(t.id, t))
      case (acc, d: Defi)               => acc.copy(balances = acc.balances.updated(d.id, d))
      case (acc, _)                     => acc
    }

    def isMultichain: Boolean = w.chain == Chain.Ethereum

    def withChain(chain: Chain): Wallet = w.copy(chain = chain)

    def withFilters(filters: Seq[TokenFilter]): Wallet = 
      w.copy(filters = filters.map(f => f.chain -> f).toMap)

    def merge(other: Wallet): Wallet =
      w.addBalances(other.balances.values.toSeq: _*)

    private def accept(t: Token): Boolean = (t.contract, w.filters.get(t.chain)) match
      case (Some(a), Some(f)) => !f.ignore.contains(a)
      case _                  => true

  def mergeAll(a: Seq[Wallet], b: Seq[Wallet]): Seq[Wallet] =
    val map = b.map(v => v.address -> v).toMap
    a.map(v => map.get(v.address).map(v.merge(_)).getOrElse(v))

  def loadWallets: IO[Seq[Wallet]] =
    for cfgDir  <- IO.pure(cryptosisDirectory(Xdg.Config))
        wallets <- loadYaml[Seq[Wallet]](cfgDir </> "wallets.yaml")
        filters <- TokenFilter.loadTokenFilters(cfgDir </> "token-filters.yaml")
    yield wallets.map(w => w.withFilters(filters))
