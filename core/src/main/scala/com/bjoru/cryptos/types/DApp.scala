package com.bjoru.cryptos.types

import cats.{Monad, Show}
import cats.implicits.given

enum DAppId:
  case Gro
  case Curve
  case TraderJoe
  case Wonderland
  case DefiKingdoms

enum DApp:
  case Staking(id: DAppId, contract: Address, staked: Seq[Token], rewards: Seq[Token])
  case Farming(id: DAppId, contract: Address, pairs: Seq[Token], rewards: Seq[Token])
  case LQPool(id: DAppId,  contract: Address, tokens: Seq[Token])

object DApp:

  given Show[DApp] = Show.show {
    case Staking(i, c, s, r) =>
      val s1 = s"Staking: $i ${s.map(_.symbol).mkString("/")} = $$${s.foldLeft(0.0)(_ + _.valueUsd)}"
      s"$s1\nRewards:\n" + r.map(_.show).mkString("  ", "\n", "")
    case Farming(i, c, p, r) =>
      val s1 = s"Farming: $i ${p.map(_.symbol).mkString("/")} = $$${p.foldLeft(0.0)(_ + _.valueUsd)}"
      s"$s1\nRewards:\n" + r.map(_.show).mkString("  ", "\n", "")
    case LQPool(i, c, t) =>
      s"Pool: $i ${t.map(_.symbol).mkString("/")} = $$${t.foldLeft(0.0)(_ + _.valueUsd)}"
  }

  extension (dapp: DApp)

    def id: DAppId = dapp match
      case s: Staking => s._1
      case f: Farming => f._1
      case l: LQPool  => l._1

    def contract: Address = dapp match
      case s: Staking => s._2
      case f: Farming => f._2
      case l: LQPool  => l._2

    def unpricedTokens: Seq[Token] = dapp match
      case Staking(_, _, s, r) => 
        (s ++ r).distinctBy(Token.TokenId).filter(_.priceUsd.isEmpty)
      case Farming(_, _, p, r) => 
        (p ++ r).distinctBy(Token.TokenId).filter(_.priceUsd.isEmpty)
      case LQPool(_, _, t) => 
        t.distinctBy(Token.TokenId).filter(_.priceUsd.isEmpty)

    def valueUsd: Double = dapp match 
      case Staking(_, _, s, r) => Token.valueUsd(s ++ r)
      case Farming(_, _, p, r) => Token.valueUsd(p ++ r)
      case LQPool(_, _, t)     => Token.valueUsd(t)

    def update(f: Token => Token): DApp = dapp match
      case v: Staking => v.copy(
        staked  = v.staked.map(f),
        rewards = v.rewards.map(f)
      )
      case v: Farming => v.copy(
        pairs   = v.pairs.map(f),
        rewards = v.rewards.map(f)
      )
      case v: LQPool => v.copy(tokens = v.tokens.map(f))

    def updateM[F[_]: Monad](f: Token => F[Token]): F[DApp] = dapp match
      case v: Staking => 
        for a <- v.staked.traverse(f)
            b <- v.rewards.traverse(f)
        yield v.copy(staked = a, rewards = b)
      case v: Farming =>
        for a <- v.pairs.traverse(f)
            b <- v.rewards.traverse(f)
        yield v.copy(pairs = a, rewards = b)
      case v: LQPool =>
        v.tokens.traverse(f).map(r => v.copy(tokens = r))