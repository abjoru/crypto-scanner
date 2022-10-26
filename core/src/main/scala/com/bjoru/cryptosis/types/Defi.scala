package com.bjoru.cryptosis.types

import cats.Show
import cats.syntax.show.given
import cats.syntax.traverse.given

import com.bjoru.cryptosis.*

import scala.util.Try

// TODO only need once instance of this since price data has been externalized!
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

  given Show[Defi] = Show.show {
    case v: Stake =>
      val lname = v.liquidity.map(_.symbol.show).mkString("/")
      val lvalue = v.liquidity.traverse(_.valueUsd).map(_.reduce(_ + _)).getOrElse(Price.Zero)
      s"${v.chain} $lname ${lvalue.show} (Stake)"
    case v: Farm =>
      val lname = v.liquidity.map(_.symbol.show).mkString("/")
      val lvalue = v.liquidity.traverse(_.valueUsd).map(_.reduce(_ + _)).getOrElse(Price.Zero)
      val cname = v.claimable.map(_.symbol.show).mkString("/")
      val cvalue = v.claimable.traverse(_.valueUsd).map(_.reduce(_ + _)).getOrElse(Price.Zero)
      s"${v.chain} $lname ${lvalue.show} [$cname ${cvalue.show}] (Farm)"
    case v: Pool =>
      val lname = v.liquidity.map(_.symbol.show).mkString("/")
      val lvalue = v.liquidity.traverse(_.valueUsd).map(_.reduce(_ + _)).getOrElse(Price.Zero)
      s"${v.chain} $lname ${lvalue.show} (Pool)"
  }

  extension (d: Defi)

    def providerId: String = d match
      case v: Stake => v.providerId
      case v: Farm  => v.providerId
      case v: Pool  => v.providerId

    def name: String = d match
      case v: Stake => v.name
      case v: Farm  => v.name
      case v: Pool  => v.name

    def chain: Chain = d match
      case v: Stake => v.chain
      case v: Farm  => v.chain
      case v: Pool  => v.chain

    def unpriced: Seq[Token] = d match
      case v: Stake => v.liquidity.filterNot(_.isPriced).distinct
      case v: Farm  => (v.liquidity.filterNot(_.isPriced) ++ v.claimable.filterNot(_.isPriced).distinct)
      case v: Pool  => (v.liquidity :+ v.poolToken).filterNot(_.isPriced).distinct

    def valueUsd: Try[Price] = d match
      case v: Stake =>
        for a <- v.liquidity.traverse(_.valueUsd)
            b  = if a.isEmpty then Price.Zero else a.reduce(_ + _)
        yield b

      case v: Farm =>
        for a <- v.liquidity.traverse(_.valueUsd)
            b <- v.claimable.traverse(_.valueUsd)
            c  = if a.isEmpty then Price.Zero else a.reduce(_ + _)
            d  = if b.isEmpty then Price.Zero else b.reduce(_ + _)
        yield c + d

      case v: Pool =>
        for a <- v.liquidity.traverse(_.valueUsd)
            b  = if a.isEmpty then Price.Zero else a.reduce(_ + _)
        yield b
