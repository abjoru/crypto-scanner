package com.bjoru.cryptosis.oracles

import cats.syntax.foldable.*

import io.circe.*

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*

import scala.util.{Try, Success, Failure}

final case class GToken(
  id: String,
  name: String,
  symbol: Symbol,
  platforms: Map[Chain, Option[Address]]
)

object GToken:

  given Decoder[GToken] = Decoder.instance { hc =>
    for a <- hc.downField("id").as[String]
        b <- hc.downField("name").as[String]
        c <- hc.downField("symbol").as[Symbol]
        d <- hc.downField("platforms").as[Option[Map[String, Json]]]
        e <- decodeMap(d.getOrElse(Map.empty)).toCirce(hc)
    yield GToken(a, b, c, e)
  }

  private def decodeMap(m: Map[String, Json]): Try[Map[Chain, Option[Address]]] =
    val result = m.toSeq.foldLeftM(Seq.empty[(Chain, Option[Address])]) {
      case (acc, (k, _)) if toChain(k).isFailure => Success(acc)
      case (acc, (k, v)) if v.isNull => toChain(k).map(c => acc :+ (c -> None))
      case (acc, (k, v)) =>
        for a <- toChain(k)
            b <- v.as[String].toTry
            c <- if b.isEmpty then Success(None) else Address.fromString(b).map(Some(_))
        yield acc :+ (a -> c)
    }

    result.map(_.toMap)

  private def toChain(str: String): Try[Chain] = str match
    case "bitcoin"             => Success(Chain.Bitcoin)
    case "ethereum"            => Success(Chain.Ethereum)
    case "solana"              => Success(Chain.Solana)
    case "elrond"              => Success(Chain.Elrond)
    case "binance-smart-chain" => Success(Chain.Binance)
    case "avalanche"           => Success(Chain.Avalanche)
    case "fantom"              => Success(Chain.Fantom)
    case "polygon-pos"         => Success(Chain.Polygon)
    case "harmony-shard-0"     => Success(Chain.Harmony)
    case "dogechain"           => Success(Chain.Dogecoin)
    case "energi"              => Success(Chain.Energi)
    case "xdai"                => Success(Chain.Gnosis)
    case "aurora"              => Success(Chain.Aurora)
    case "cronos"              => Success(Chain.Cronos)
    case "smartbch"            => Success(Chain.SmartBCH)
    case "near-protocol"       => Success(Chain.NearProtocol)
    //case other                 => Failure(Exception(s"Unknown key for chain: $other"))
    case _                     => Success(Chain.Unknown)
