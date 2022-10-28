package com.bjoru.cryptosis.views

import cats.effect.IO
import cats.syntax.show.given

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*

object TokenListView:

  def render(env: Env, wallets: Seq[Wallet]): IO[Unit] = IO {
    priceTokens(env, collectTokens(wallets)).sortBy(_._1.chain).foreach {
      case (Token(_, _, s, c, _, _, b), v) => 
        val bal = s"${b.show} $s"
        println(f"$c%-10s $bal%-20s = ${v.show}%10s")
    }
  }

  def priceTokens(env: Env, tokens: Map[Id, Token]): Seq[(Token, Price)] =
    tokens.values.map(t => (t, env.priceApi.valueOf(t))).toSeq

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
