package com.bjoru.cscanner.egld

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

class ElrondProvider(endpoint: Endpoint):

  given Decoder[TokenBalance] = Decoder.instance { c =>
    for name <- c.downField("name").as[String]
        symb <- c.downField("ticker").as[String]
        dec  <- c.downField("decimals").as[Int]
        bal  <- c.downField("balance").as[String]
        res  <- Quantity.decodeTokenQuantity(Token(name, symb, dec, None), bal).circeResult(c)
    yield res
  }

  def getTokens(wallet: Wallet)(using client: Client[IO]): IO[Seq[TokenBalance]] =
    val req = GET(endpoint.uri / "account" / wallet.address.stringValue / "tokens")
    client.expect(req)(jsonOf[IO, Seq[TokenBalance]])

object ElrondProvider:

  def loadProvider(cfgDir: Path): IO[ElrondProvider] =
    loadEndpoints(cfgDir </> ENDPOINTS_FILE).flatMap { e =>
      Endpoint.select(Provider.Elrond, Chain.Elrond)(e) match
        case Some(ep) => IO.pure(ElrondProvider(ep))
        case None     => IO.raiseError(new Exception("No Elrond provider configured!"))
    }
