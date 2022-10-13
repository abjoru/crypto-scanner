package com.bjoru.cscanner.eth

import cats.syntax.traverse.given
import cats.effect.IO

import io.circe.*
import io.circe.syntax.{*, given}

import org.http4s.dsl.io.*
import org.http4s.client.*
import org.http4s.client.dsl.io.*
import org.http4s.circe.*

import com.bjoru.cscanner.{*, given}
import com.bjoru.cscanner.types.*
import com.bjoru.cscanner.utils.*
import com.bjoru.cscanner.config.*

import java.nio.file.Path

class QuickNodeProvider(endpoint: Endpoint):

  given Decoder[TokenBalance] = Decoder.instance { c =>
    for name <- c.downField("name").as[String]
        symb <- c.downField("symbol").as[Symbol]
        dec  <- c.downField("decimals").as[Int]
        addr <- c.downField("address").as[Address]
        bal  <- c.downField("amount").as[String]
        res  <- Quantity.decodeTokenQuantity(Token(symb, name, dec, Some(addr)), bal).circeResult(c)
    yield res
  }

  given Encoder[Request.EthBalance] = Encoder.encodeJson.contramap {
    case Request.EthBalance(wallet) => Json.obj(
      "id"      -> 1.asJson,
      "jsonrpc" -> "2.0".asJson,
      "method"  -> "eth_getBalance".asJson,
      "params"  -> List(wallet.address.stringValue, "latest").asJson
    )
  }

  given Encoder[Request.TokenBalances] = Encoder.encodeJson.contramap {
    case Request.TokenBalances(wallet, tokens) => Json.obj(
      "id"      -> 67.asJson,
      "jsonrpc" -> "2.0".asJson,
      "method"  -> "qn_getWalletTokenBalance".asJson,
      "params"  -> Json.obj(
        "wallet"    -> wallet.address.stringValue.asJson,
        "contracts" -> tokenList(tokens, Some(100))
      )
    )
  }

  def getEthBalance(wallet: Wallet)(using client: Client[IO]): IO[TokenBalance] =
    val req = POST(Request.EthBalance(wallet).asJson, endpoint.uri / endpoint.apiKey)
    client.expect(req)(jsonOf[IO, Json]).flatMap { json =>
      for hex <- IO.fromEither(json.hcursor.downField("result").as[String])
          wei <- IO.fromTry(Quantity.decodeQuantity(hex))
      yield TokenBalance(Token.Eth, fromWei(wei, Unit.Ether))
    }

  def getTokenBalance(wallet: Wallet, tokens: Set[Token])(using client: Client[IO]): IO[Seq[TokenBalance]] =
    val req = POST(Request.TokenBalances(wallet, tokens).asJson, endpoint.uri / endpoint.apiKey)
    client.expect(req)(jsonOf[IO, Json]).flatMap { json =>
      IO.fromEither(json.hcursor.downField("result").downField("assets").as[Seq[TokenBalance]])
    }

  def tokenList(tokens: Set[Token], limit: Option[Int] = None) = 
    val xs = tokens.toSeq.collect {
      case Token(_, _, _, Some(contract), _) => contract.asJson
    }

    limit match
      case Some(n) => xs.take(n).asJson
      case None    => xs.asJson

object QuickNodeProvider:

  def loadProvider(cfgDir: Path, chain: Chain): IO[QuickNodeProvider] = 
    loadEndpoints(cfgDir </> "endpoints.yaml").flatMap { e =>
      Endpoint.select(Provider.QuickNode, chain)(e) match
        case Some(ep) => IO.pure(QuickNodeProvider(ep))
        case None     => IO.raiseError(new Exception(s"No QuickNode provider for $chain"))
    }
