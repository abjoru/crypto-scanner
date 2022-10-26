package com.bjoru.cryptosis.types

import cats.Show
import cats.syntax.show.given

import io.circe.*
import io.circe.syntax.*

import com.bjoru.cryptosis.*

import scala.util.{Try, Success, Failure}

enum Token:

  case Base(
    geckoId:  String,
    name:     String,
    symbol:   Symbol,
    chain:    Chain,
    contract: Option[Address]
  )

  case Balanced(
    geckoId:  String,
    name:     String,
    symbol:   Symbol,
    chain:    Chain,
    contract: Option[Address],
    decimals: Int,
    balance:  Balance
  )

  case Priced(
    geckoId:  String,
    name:     String,
    symbol:   Symbol,
    chain:    Chain,
    contract: Option[Address],
    decimals: Int,
    balance:  Balance,
    price:    Price
  )

object Token:

  given Identity[Token] with
    extension (t: Token) def id = (t.symbol, t.chain, t.contract) match
      case (s, c, Some(a)) => Id.create(s.lower, c, a)
      case (s, c, None)    => Id.create(s.lower, c)

  given Show[Token] = Show.show {
    case v: Base     => s"${v.chain} ${v.symbol.show}"
    case v: Balanced => s"${v.chain} ${v.balance.show} ${v.symbol.show}"
    case v: Priced   => s"${v.chain} ${v.balance.show} ${v.symbol.show} @ ${v.price.show} = ${v.valueUsd.getOrElse(Price.Zero).show} (${v.contract.map(_.show).getOrElse("")})"
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
    yield Balanced(i, n, s, c, a, d, Balance.Zero)
  }

  extension (t: Token)

    def geckoId: String = t match
      case v: Base     => v.geckoId
      case v: Balanced => v.geckoId
      case v: Priced   => v.geckoId

    def name: String = t match
      case v: Base     => v.name
      case v: Balanced => v.name
      case v: Priced   => v.name

    def symbol: Symbol = t match
      case v: Base     => v.symbol
      case v: Balanced => v.symbol
      case v: Priced   => v.symbol

    def chain: Chain = t match
      case v: Base     => v.chain
      case v: Balanced => v.chain
      case v: Priced   => v.chain

    def contract: Option[Address] = t match
      case v: Base     => v.contract
      case v: Balanced => v.contract
      case v: Priced   => v.contract

    def decimals: Int = t match
      case v: Base     => 18 // default
      case v: Balanced => v.decimals
      case v: Priced   => v.decimals

    def isPriced: Boolean = t match
      case v: Priced => true
      case _         => false

    def isZero: Boolean = t match
      case v: Priced => v.valueUsd.map(_ == Price.Zero).getOrElse(true)
      case _         => true

    def isDust: Boolean = t match
      case v: Priced => v.valueUsd.map(_ < Price(0.1)).getOrElse(true)
      case _         => true

    def withName(name: String): Token = t match
      case v: Base     => v.copy(name = name)
      case v: Balanced => v.copy(name = name)
      case v: Priced   => v.copy(name = name)

    def withSymbol(symbol: Symbol): Token = t match
      case v: Base     => v.copy(symbol = symbol)
      case v: Balanced => v.copy(symbol = symbol)
      case v: Priced   => v.copy(symbol = symbol)

    def withChain(chain: Chain): Token = t match
      case v: Base     => v.copy(chain = chain)
      case v: Balanced => v.copy(chain = chain)
      case v: Priced   => v.copy(chain = chain)

    def withBalance(decimals: Int, balance: Balance): Token = t match
      case Base(i, n, s, c, a) => Balanced(i, n, s, c, a, decimals, balance)
      case v: Balanced => v.copy(decimals = decimals, balance = balance)
      case v: Priced   => v.copy(decimals = decimals, balance = balance)

    def withPrice(price: Price): Token = t match
      case Base(i, n, s, c, a) => Priced(i, n, s, c, a, 18, Balance.Zero, price)
      case Balanced(i, n, s, c, a, d, b) => Priced(i, n, s, c, a, d, b, price)
      case v: Priced => v.copy(price = price)

    def basedOn(token: Token): Token = token match
      case v: Base => t
      case v: Balanced => v.copy(
        geckoId  = t.geckoId,
        name     = t.name,
        symbol   = t.symbol,
        contract = t.contract.orElse(v.contract)
      )
      case v: Priced => v.copy(
        geckoId  = t.geckoId,
        name     = t.name,
        symbol   = t.symbol,
        contract = t.contract.orElse(v.contract)
      )

    def valueUsd: Try[Price] = t match
      case v: Priced => Success(v.price * v.balance)
      case _ => Failure(Exception(s"No price defined for ${t.show}"))

  def fromExchange(symbol: Symbol, chain: Chain, balance: Balance): Token =
    Balanced(symbol.lower, symbol.cap, symbol, chain, None, 18, balance)
