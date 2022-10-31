package com.bjoru.cryptosis.views

import cats.effect.IO
import cats.syntax.show.given

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*

object DefiListView:

  def render(wallets: Seq[Wallet]): SIO[Unit] = SIO.inspect { state =>
    priceDefi(state, collectDefi(wallets)).foreach {
      case (d, v) => println(s"${d.show} = ${v.show}")
    }
  }

  def priceDefi(state: State, defi: Map[Id, Defi]) =
    defi.values.map(d => (d, state.valueOf(d)))

  def collectDefi(wallets: Seq[Wallet]): Map[Id, Defi] =
    wallets.foldLeft(Map.empty[Id, Defi])((a, b) => populateMap(b, a))

  def populateMap(wallet: Wallet, defi: Map[Id, Defi]): Map[Id, Defi] =
    wallet.balances.foldLeft(defi) {
      case (acc, (i, d: Defi)) if acc.contains(i) =>
        println(s"WARN: duplicate defi protocol: ${d.show} and ${acc.get(i).map(_.show)}")
        acc
      case (acc, (i, d: Defi)) => acc + (i -> d)
      case (acc, _) => acc
    }
