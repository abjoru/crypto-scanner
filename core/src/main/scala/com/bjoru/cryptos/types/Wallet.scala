package com.bjoru.cryptos.types

import cats.{Monad, Show}
import cats.implicits.given
import cats.effect.IO

import pureconfig.*

import com.bjoru.cryptos.config.*

import java.nio.file.Path

final case class Wallet private (
  name:    String,
  chain:   Chain,
  address: Address,
  tokens:  Seq[Token],
  dapps:   Seq[DApp]
)

object Wallet:

  given Show[Wallet] = Show.show { wallet =>
    val tList = wallet.tokens.map(_.show).mkString("\n")
    val dList = wallet.dapps.map(_.show).mkString("\n")

    val s1 = s"${wallet.name} [${wallet.address}] ($$${wallet.valueUsd}):"

    val s2 = if wallet.tokens.isEmpty
      then s1
      else s1 + "\nTokens:\n" + tList
    
    if wallet.dapps.isEmpty
      then  s2
      else s2 + "\nDApps:\n" + dList
  }

  given ConfigReader[Wallet] = ConfigReader.forProduct3("chain", "name", "address")(
    (chain: Chain, name: String, address: Address) => Wallet(name, chain, address)
  )

  extension (w: Wallet)

    def withChain(chain: Chain): Wallet =
      w.copy(chain = chain)

    def withTokens(tx: Seq[Token]): Wallet =
      // blindly drop wallet token duplicates
      w.copy(tokens = (w.tokens ++ tx).distinctBy(Token.TokenId))

    def withDApps(apps: Seq[DApp]): Wallet =
      // blindly skip app if already present (by contract address)
      w.copy(dapps = (w.dapps ++ apps).distinctBy(_.contract))

    def unpricedTokens: Seq[Token] = 
      val t1 = w.tokens.filter(_.priceUsd.isEmpty)
      val t2 = w.dapps.foldLeft(Seq.empty[Token]) {
          case (acc, app) => acc ++ app.unpricedTokens
        }

      (t1 ++ t2).distinctBy(Token.TokenId)

    def valueUsd: Double = 
      val tPrice = w.tokens.foldLeft(0.0)(_ + _.priceUsd.getOrElse(0.0))
      val dPrice = w.dapps.foldLeft(0.0)(_ + _.valueUsd)
      tPrice + dPrice

    def isMultichain: Boolean = w.chain == Chain.Ethereum

    def dappGroups: Map[DAppId, Seq[DApp]] =
      w.dapps.groupBy(_.id)

    def update(f: Token => Token): Wallet = w.copy(
      tokens = w.tokens.map(f),
      dapps  = w.dapps.map(_.update(f))
    )

    def updateM[F[_]: Monad](f: Token => F[Token]): F[Wallet] =
      for a <- w.tokens.traverse(f)
          b <- w.dapps.traverse(_.updateM[F](f))
      yield w.copy(tokens = a, dapps = b)

  def apply(name: String, chain: Chain, address: Address): Wallet =
    new Wallet(name, chain, address, Seq.empty, Seq.empty)

  def loadWallets(path: Path): IO[Seq[Wallet]] = 
    loadYamlFile[Seq[Wallet]](path)
