package com.bjoru.cryptosis.views

import cats.effect.IO
import cats.syntax.show.given
import cats.syntax.traverse.given

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*

object PortfolioTotalsView:

  def render(wallets: Seq[Wallet]): SIO[Unit] = SIO.inspectF { state =>
    val price = wallets.map(w => state.valueOf(w)) match
      case xs if xs.nonEmpty => xs.reduce(_ + _)
      case xs                => Price.Zero

    IO(println(s"Totals: ${price.show}"))
  }
