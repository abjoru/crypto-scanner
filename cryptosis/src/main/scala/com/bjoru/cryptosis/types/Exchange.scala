package com.bjoru.cryptosis.types

import cats.syntax.traverse.given

import scala.util.Try

final case class Exchange(
  name:     String,
  balances: Map[Id, Token | Defi]
)

object Exchange:

  extension (e: Exchange)

    def addBalance(item: Token | Defi): Exchange = item match
      case t: Token => e.copy(balances = e.balances.updated(t.id, t))
      case d: Defi  => e.copy(balances = e.balances.updated(d.id, d))

    def addBalances(items: Seq[Token | Defi]): Exchange = items.foldLeft(e) {
      case (acc, t: Token) => acc.copy(balances = acc.balances.updated(t.id, t))
      case (acc, d: Defi)  => acc.copy(balances = acc.balances.updated(d.id, d))
    }

    def valueUsd: Try[Price] =
      val values = e.balances.toSeq.traverse {
        case (_, t: Token) => t.valueUsd
        case (_, d: Defi)  => d.valueUsd
      }

      values.map {
        case xs if xs.isEmpty => Price.Zero
        case xs               => xs.reduce(_ + _)
      }
