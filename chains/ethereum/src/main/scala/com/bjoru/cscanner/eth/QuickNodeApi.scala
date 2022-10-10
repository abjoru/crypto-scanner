package com.bjoru.cscanner.eth

import cats.syntax.traverse.given
import cats.effect.IO

import io.circe.{*, given}
import io.circe.generic.semiauto.{*, given}

import org.http4s.Uri
import org.http4s.dsl.io.*
import org.http4s.circe.*
import org.http4s.client.*
import org.http4s.client.dsl.io.*

import pureconfig.*
import pureconfig.generic.derivation.default.*

import com.bjoru.cscanner.config.loadConfigFile
import com.bjoru.cscanner.utils.{Quantity, Unit, fromWei}
import com.bjoru.cscanner.types.{Address, Wallet, Token, TokenBalance}

import scala.util.Try

import java.nio.file.Path

given ConfigReader[Uri] = ConfigReader.fromString { str => 
  Uri.fromString(str).left.map(e => error.CannotConvert(str, "Uri", e.getMessage)) 
}

final case class QuickNodeApi(endpoint: Uri, apiKey: String) derives ConfigReader

object QuickNodeApi: 

  val tokenBalanceDecoder: Decoder[TokenBalance] = 
    Decoder.forProduct5("name", "symbol", "decimals", "address", "amount")(
      (name: String, symb: String, dec: Int, contr: Address, am: String) =>
        TokenBalance(Token(name, symb, dec, Some(contr)), BigDecimal(am) / dec)
    )

  def loadApi(path: Path): IO[QuickNodeApi] = loadConfigFile[QuickNodeApi](path)

  extension (api: QuickNodeApi)

    def ethBalance(wallet: Wallet)(using client: Client[IO]): IO[TokenBalance] = 
      val req = POST(Payloads.QuickNode.getEthBalance(wallet), api.endpoint / api.apiKey)
      client.expect(req)(jsonOf[IO, Json]).flatMap { json =>
        for hex <- IO.fromEither(json.hcursor.downField("result").as[String])
            wei <- IO.fromTry(Quantity.decodeQuantity(hex))
        yield TokenBalance(Token.Eth, fromWei(wei, Unit.Ether))
      }

    def tokenBalance(wallet: Wallet, tokens: Set[Token])(using client: Client[IO]): IO[Seq[TokenBalance]] =
      val req = POST(Payloads.QuickNode.getWalletTokenBalance(wallet, tokens), api.endpoint / api.apiKey)
      client.expect(req)(jsonOf[IO, Json]).flatMap { json =>
        for arr <- IO.fromEither(json.hcursor.downField("result").downField("assets").as[List[Json]])
            tok <- IO.fromEither(arr.traverse(_.as[TokenBalance](using tokenBalanceDecoder)))
        yield tok
      }
