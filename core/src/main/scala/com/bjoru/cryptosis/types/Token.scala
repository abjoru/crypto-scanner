package com.bjoru.cryptosis.types

import cats.Show
import cats.syntax.show.given

import io.circe.*
import io.circe.syntax.*

import com.bjoru.cryptosis.*

import scala.util.{Try, Success, Failure}

final case class Token(
  geckoId:  String,
  name:     String,
  symbol:   Symbol,
  chain:    Chain,
  contract: Option[Address],
  decimals: Int,
  balance:  Balance
)

// Do not add encoders/decoders for tokens here as
// providers will need to define their own versions
// that fits their datasets.
object Token:

  given Identity[Token] with
    extension (t: Token) def id = mkId(t.symbol, t.chain)

  given Show[Token] = Show.show { v =>
    s"${v.chain} ${v.balance.show} ${v.symbol.show} ${v.contract.map(a => s"(${a.show})").getOrElse("")}"
  }

  extension (t: Token)

    def withName(name: String): Token =
      t.copy(name = name)

    def withSymbol(symbol: Symbol): Token =
      t.copy(symbol = symbol)

    def withChain(chain: Chain): Token =
      t.copy(chain = chain)

    def withBalance(balance: Balance): Token =
      t.copy(balance = balance)

    def addBalance(balance: Balance): Token =
      t.copy(balance = t.balance + balance)

    def basedOn(token: Token): Token = token.copy(
      geckoId  = t.geckoId,
      name     = t.name,
      symbol   = t.symbol,
      contract = t.contract.orElse(token.contract)
    )

  def fromExchange(symbol: Symbol, chain: Chain, balance: Balance): Token =
    Token(symbol.lower, symbol.cap, symbol, chain, None, 18, balance)

  def mkId(symbol: Symbol, chain: Chain): Id = 
    Id.create(symbol, chain)

  def simple(name: String, sym: Symbol, chain: Chain): Token =
    Token(name.toLowerCase, name, sym, chain, None, 18, Balance.Zero)
