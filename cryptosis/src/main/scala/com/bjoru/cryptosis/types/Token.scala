package com.bjoru.cryptosis.types

import io.circe.*
import io.circe.syntax.*

import com.bjoru.cryptosis.*

enum Token:
  case BaseToken(
    id:       String,
    name:     String,
    symbol:   Symbol,
    chain:    Chain,
    contract: Option[Address]
  )

  case BalancedToken(
    id:       String,
    name:     String,
    symbol:   Symbol,
    chain:    Chain,
    decimals: Int,
    contract: Option[Address],
    balance:  Balance
  )

  case PricedAndBalancedToken(
    id:       String,
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
    extension (t: Token) def id = t match
      case v: BaseToken              => Id.createRaw(v.id)
      case v: BalancedToken          => Id.createRaw(v.id)
      case v: PricedAndBalancedToken => Id.createRaw(v.id)

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
