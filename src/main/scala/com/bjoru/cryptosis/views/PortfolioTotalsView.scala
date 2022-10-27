package com.bjoru.cryptosis.views

import cats.effect.IO
import cats.syntax.show.given
import cats.syntax.traverse.given

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*

object PortfolioTotalsView:

  def render(env: Env, wallets: Seq[Wallet]): IO[Unit] = 
    val price = wallets.map(w => env.priceApi.valueOf(w)) match
      case xs if xs.nonEmpty => xs.reduce(_ + _)
      case xs                => Price.Zero

    IO(println(s"Totals: ${price.show}"))
