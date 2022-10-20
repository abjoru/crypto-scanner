package com.bjoru.cryptosis.types

import cats.syntax.traverse.given

import com.bjoru.cryptosis.*

import scala.util.Try

trait DApp:

  val providerId: String

  val name: String

  val liquidity: Seq[Token]

  val claimable: Seq[Token]

  def unpricedTokens: Set[Token] =
    liquidity.toSet ++ claimable.toSet

  def valueUsd: Try[Price] = 
    for lq <- liquidity.traverse(_.valueUsd)
        cm <- claimable.traverse(_.valueUsd)
    yield lq.reduce(_ + _) + cm.reduce(_ + _)

object DApp:

  given Identity[DApp] with
    extension (d: DApp) def id = 
      val lqSym = d.liquidity.map(_.symbol.upper).sorted
      val cmSym = d.claimable.map(_.symbol.upper).sorted
      Id.create((lqSym ++ cmSym): _*)
