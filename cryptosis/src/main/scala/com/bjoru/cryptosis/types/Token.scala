package com.bjoru.cryptosis.types

import cats.Show
import cats.syntax.show.given

import io.circe.*
import io.circe.syntax.*

import com.bjoru.cryptosis.*

import scala.util.{Try, Success, Failure}

enum Token:
  case BaseToken(
    geckoId:  String,
    name:     String,
    symbol:   Symbol,
    chain:    Chain,
    contract: Option[Address]
  )

  case BalancedToken(
    geckoId:  String,
    name:     String,
    symbol:   Symbol,
    chain:    Chain,
    decimals: Int,
    contract: Option[Address],
    balance:  Balance
  )

  case PricedAndBalancedToken(
    geckoId:  String,
    name:     String,
    symbol:   Symbol,
    chain:    Chain,
    decimals: Int,
    contract: Option[Address],
    balance:  Balance,
    price:    Price
  )

object Token:

  given Identity[Token] with
    extension (t: Token) def id = (t.symbol, t.chain, t.contract) match
      case (s, c, Some(a)) => Id.create(s.lower, c.str, a.str)
      case (s, c, None)    => Id.create(s.lower, c.str)

  given Show[Token] = Show.show {
    case v: BaseToken              => s"${v.chain} ${v.symbol.show}"
    case v: BalancedToken          => s"${v.chain} ${v.balance.show} ${v.symbol.show}"
    case v: PricedAndBalancedToken => s"${v.chain} ${v.balance.show} ${v.symbol.show} @ ${v.price.show} = ${v.valueUsd.getOrElse(Price.Zero).show} (${v.contract.map(_.str).getOrElse("")})"
  }

  given Encoder[Token] = Encoder.instance { token =>
    Json.obj(
      "id"       -> token.id.asJson,
      "name"     -> token.name.asJson,
      "symbol"   -> token.symbol.asJson,
      "chain"    -> token.chain.asJson,
      "decimals" -> token.decimals.asJson,
      "contract" -> token.contract.asJson
    )
  }

  given Decoder[Token] = Decoder.instance { cursor =>
    for i <- cursor.downField("id").as[String]
        n <- cursor.downField("name").as[String]
        s <- cursor.downField("symbol").as[Symbol]
        c <- cursor.downField("chain").as[Chain]
        d <- cursor.downField("decimals").as[Int]
        a <- cursor.downField("contract").as[Option[Address]]
    yield BalancedToken(i, n, s, c, d, a, Balance.Zero)
  }

  extension (t: Token)

    def geckoId: String = t match
      case v: BaseToken              => v.geckoId
      case v: BalancedToken          => v.geckoId
      case v: PricedAndBalancedToken => v.geckoId

    def name: String = t match
      case v: BaseToken              => v.name
      case v: BalancedToken          => v.name
      case v: PricedAndBalancedToken => v.name

    def symbol: Symbol = t match
      case v: BaseToken              => v.symbol
      case v: BalancedToken          => v.symbol
      case v: PricedAndBalancedToken => v.symbol

    def chain: Chain = t match
      case v: BaseToken              => v.chain
      case v: BalancedToken          => v.chain
      case v: PricedAndBalancedToken => v.chain

    def decimals: Int = t match
      case v: BaseToken              => 18
      case v: BalancedToken          => v.decimals
      case v: PricedAndBalancedToken => v.decimals

    def contract: Option[Address] = t match
      case v: BaseToken              => v.contract
      case v: BalancedToken          => v.contract
      case v: PricedAndBalancedToken => v.contract

    def missingPrice: Boolean = t match
      case v: PricedAndBalancedToken => v.price == Price.Zero
      case _                         => true

    def valueUsd: Try[Price] = t match
      case v: PricedAndBalancedToken => 
        Success(v.price * v.balance.toBigDecimal)
      case _ =>
        Failure(new Exception(s"No price defined for ${t.show}"))

    def withChain(chain: Chain): Token = t match
      case v: BaseToken              => v.copy(chain = chain)
      case v: BalancedToken          => v.copy(chain = chain)
      case v: PricedAndBalancedToken => v.copy(chain = chain)

    def withBalance(decimals: Int, balance: Balance): Token = t match
      case BaseToken(i, n, s, c, a) =>
        BalancedToken(i, n, s, c, decimals, a, balance)
      case v: BalancedToken =>
        v.copy(balance = balance)
      case v: PricedAndBalancedToken =>
        v.copy(balance = balance)

    def withPrice(price: Price): Token = t match
      case BaseToken(i, n, s, c, a) =>
        PricedAndBalancedToken(i, n, s, c, 18, a, Balance.Zero, price)
      case BalancedToken(i, n, s, c, d, a, b) =>
        PricedAndBalancedToken(i, n, s, c, d, a, b, price)
      case v: PricedAndBalancedToken =>
        v.copy(price = price)

    def base(baseToken: Token): Token = baseToken match
      case v: BaseToken => t
      case v: BalancedToken => v.copy(
        geckoId = t.geckoId,
        name    = t.name,
        symbol  = t.symbol,
        contract = t.contract.orElse(v.contract)
      )
      case v: PricedAndBalancedToken => v.copy(
        geckoId = t.geckoId,
        name    = t.name,
        symbol  = t.symbol,
        contract = t.contract.orElse(v.contract)
      )

    def isEmpty: Boolean = t match
      case v: BalancedToken          => v.balance.isEmpty
      case v: PricedAndBalancedToken => v.balance.isEmpty
      case _                         => true

  def apply(
    id:       String,
    name:     String, 
    symbol:   Symbol, 
    chain:    Chain, 
    contract: Option[Address]
  ): Token = BaseToken(id, name, symbol, chain, contract)

  def apply(
    id:       String,
    name:     String, 
    symbol:   Symbol, 
    chain:    Chain, 
    decimals: Int,
    contract: Option[Address]
  ): Token = BalancedToken(id, name, symbol, chain, decimals, contract, Balance.Zero)

  def apply(
    id:       String,
    name:     String,
    symbol:   Symbol,
    chain:    Chain,
    decimals: Int,
    contract: Address,
    balance:  Balance,
    price:    Price
  ): Token = PricedAndBalancedToken(id, name, symbol, chain, decimals, Some(contract), balance, price)
