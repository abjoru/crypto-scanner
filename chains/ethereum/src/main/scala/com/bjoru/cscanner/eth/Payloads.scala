package com.bjoru.cscanner.eth

import com.bjoru.cscanner.types.{Wallet, Token}

import io.circe.*
import io.circe.syntax.{*, given}

object Payloads:

  def tokenList(tokens: Set[Token], limit: Option[Int] = None) = 
    val xs = tokens.toSeq.collect {
      case Token(_, _, _, Some(contract)) => contract.asJson
    }

    limit match
      case Some(n) => xs.take(n).asJson
      case None    => xs.asJson

  object QuickNode:

    def getEthBalance(wallet: Wallet) = Json.obj(
      "id"      -> 1.asJson,
      "jsonrpc" -> "2.0".asJson,
      "method"  -> "eth_getBalance".asJson,
      "params"  -> List(wallet.address.stringValue, "latest").asJson
    )

    def getWalletTokenBalance(wallet: Wallet, tokens: Set[Token]) = Json.obj(
      "id"      -> 67.asJson,
      "jsonrpc" -> "2.0".asJson,
      "method"  -> "qn_getWalletTokenBalance".asJson,
      "params"  -> Json.obj(
        "wallet"    -> wallet.address.stringValue.asJson,
        "contracts" -> tokenList(tokens, Some(100))
      )
    )
