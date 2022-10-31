package com.bjoru.cryptosis.views

import cats.effect.IO
import cats.syntax.show.given

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*

object TokenListView:

  def render(wallets: Seq[Wallet], filterDust: Boolean = true): SIO[Unit] = SIO.inspect { state =>
    priceTokens(state, collectTokens(wallets), filterDust).sortBy(_._1.chain).foreach {
      case (Token(_, _, s, c, _, _, b), v) => 
        val bal = s"${b.show} $s"
        println(f"$c%-10s $bal%-20s = ${v.show}%10s")
    }
  }

  def priceTokens(state: State, tokens: Map[Id, Token], fDust: Boolean): Seq[(Token, Price)] =
    tokens.values.map(t => (t, state.valueOf(t))).toSeq.filter {
      case t if fDust => t._2 > Price(0.01)
      case t          => true
    }

  def collectTokens(wallets: Seq[Wallet]): Map[Id, Token] =
    wallets.foldLeft(Map.empty[Id, Token]) {
      case (acc, w) => populateMap(w, acc)
    }

  def populateMap(wallet: Wallet, tokens: Map[Id, Token]): Map[Id, Token] =
    wallet.balances.foldLeft(tokens) {
      case (acc, (i, t: Token)) => acc.updatedWith(i) {
        case Some(o) => Some(o.addBalance(t.balance))
        case None    => Some(t)
      }
      case (acc, _) => acc
    }
