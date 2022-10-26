package com.bjoru.cryptosis.types

import cats.Show
import cats.syntax.show.given
import cats.syntax.traverse.given

import com.bjoru.cryptosis.*

import scala.util.Try

enum Defi:

  case Stake(
    providerId: String,
    name:       String,
    chain:      Chain,
    liquidity:  Seq[Token]
  )

  case Farm(
    providerId: String,
    name:       String,
    chain:      Chain,
    liquidity:  Seq[Token],
    claimable:  Seq[Token]
  )

  case Pool(
    providerId: String,
    name:       String,
    chain:      Chain,
    liquidity:  Seq[Token],
    poolToken:  Token
  )

object Defi:

  given Identity[Defi] with
    extension (d: Defi) def id = d match
      case v: Stake =>
        Id.create(v.liquidity.map(_.symbol.upper).sorted: _*)
      case v: Farm =>
        val lqSym = v.liquidity.map(_.symbol.upper).sorted
        val clSym = v.claimable.map(_.symbol.upper).sorted
        Id.create((lqSym ++ clSym): _*)
      case v: Pool => 
        val ids = v.liquidity.map(_.symbol.upper).sorted
        Id.create((v.poolToken.symbol.upper +: ids): _*)
