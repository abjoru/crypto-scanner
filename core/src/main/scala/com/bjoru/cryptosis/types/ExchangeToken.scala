package com.bjoru.cryptosis.types

import com.bjoru.cryptosis.*

final case class ExchangeToken(
  symbol:  Symbol,
  chain:   Chain,
  balance: Balance
)

object ExchangeToken:

  given Identity[ExchangeToken] with
    extension (t: ExchangeToken) 
      def id = Token.mkId(t.symbol, t.chain)
