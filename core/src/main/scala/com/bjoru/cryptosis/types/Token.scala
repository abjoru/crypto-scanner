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

object Token:

  given Identity[Token] with
    extension (t: Token) def id = mkId(t.symbol, t.chain)

  given Show[Token] = Show.show { v =>
    s"${v.chain} ${v.balance.show} ${v.symbol.show} ${v.contract.map(a => s"(${a.show})").getOrElse("")}"
  }

  given Encoder[Token] = Encoder.instance { token =>
    Json.obj(
      "id"       -> token.geckoId.asJson,
      "name"     -> token.name.asJson,
      "symbol"   -> token.symbol.asJson,
      "chain"    -> token.chain.asJson,
      "decimals" -> token.decimals.asJson,
      "contract" -> token.contract.asJson
    )
  }

  given Decoder[Token] = Decoder.instance { hc =>
    for i <- hc.downField("id").as[String]
        n <- hc.downField("name").as[String]
        s <- hc.downField("symbol").as[Symbol]
        c <- hc.downField("chain").as[Chain]
        d <- hc.downField("decimals").as[Int]
        a <- hc.downField("contract").as[Option[Address]]
    yield Token(i, n, s, c, a, d, Balance.Zero)
  }

  extension (t: Token)

    def withName(name: String): Token =
      t.copy(name = name)

    def withSymbol(symbol: Symbol): Token =
      t.copy(symbol = symbol)

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
