package com.bjoru.cryptosis.types

import cats.syntax.traverse.given

import scala.util.Try

final case class Exchange(
  name:     String,
  balances: Map[Id, Token | Defi]
)

object Exchange:

  extension (e: Exchange)

    def addBalances(items: (Token | Defi)*): Exchange = items.foldLeft(e) {
      case (acc, t: Token) => acc.copy(balances = acc.balances.updated(t.id, t))
      case (acc, d: Defi)  => acc.copy(balances = acc.balances.updated(d.id, d))
    }

  def apply(name: String, tokens: Seq[Token], defi: Seq[Defi]): Exchange =
    val tokenMap = tokens.map(v => v.id -> v).toMap
    val defiMap  = defi.map(v => v.id -> v).toMap

    val result: Map[Id, Token | Defi] = tokenMap ++ defiMap

    Exchange(name, result)
