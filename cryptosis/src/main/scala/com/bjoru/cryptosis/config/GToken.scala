package com.bjoru.cryptosis.config

import cats.effect.IO
import cats.syntax.traverse.given

import io.circe.*
import io.circe.syntax.given

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*

final case class GToken(
  id: String,
  symbol: Symbol,
  name: String,
  platforms: Map[Chain, Option[Address]]
)

object GToken:

  given KeyEncoder[Chain] = new KeyEncoder[Chain]:
    override def apply(c: Chain): String = c match
      case Chain.Bitcoin   => "bitcoin"
      case Chain.Ethereum  => "ethereum"
      case Chain.Solana    => "solana"
      case Chain.Elrond    => "elrond"
      case Chain.Binance   => "binance-smart-chain"
      case Chain.Avalanche => "avalanche"
      case Chain.Fantom    => "fantom"
      case Chain.Polygon   => "polygon-pos"
      case Chain.Harmony   => "harmony-shard-0"
      case Chain.Dogecoin  => "dogechain"
      case Chain.Polkadot  => "polkadot"
      case Chain.Cardano   => "cardano"
      case Chain.Arbitrum  => "arbitrum-one"
      case Chain.Optimism  => "optimistic-ethereum"
      case Chain.Moonriver => "moonriver"
      case Chain.Aurora    => "aurora"
      case Chain.Celo      => "celo"
      case Chain.Gnosis    => "xdai"
      case Chain.Unknown   => "<unknown>"


  given Encoder[GToken] = Encoder.instance { g =>
    Json.obj(
      "id"        -> Json.fromString(g.id),
      "symbol"    -> Json.fromString(g.symbol.toString.toLowerCase),
      "name"      -> Json.fromString(g.name),
      "platforms" -> g.platforms.asJson
    )
  }

  given Decoder[GToken] = Decoder.instance { hc =>
    for a <- hc.downField("id").as[String]
        b <- hc.downField("symbol").as[Symbol]
        c <- hc.downField("name").as[String]
        d <- hc.downField("platforms").as[Json].map(decodeMap) //.as[Map[Chain, Address]]
    yield GToken(a, b, c, d)
  }

  private def decodeMap(json: Json): Map[Chain, Option[Address]] = 
    val keys = json.hcursor.keys.map(_.toList).getOrElse(List.empty)

    val initList = keys.traverse { key =>
      json.hcursor.downField(key).as[Option[Address]].map(key -> _)
    }

    val chainMap = initList.map(_.map(v => keyToChain(v._1).map(_ -> v._2)).flatten.toMap)

    chainMap.getOrElse(Map.empty)

  def keyToChain(key: String): Option[Chain] = key match
    case "bitcoin"             => Some(Chain.Bitcoin)
    case "ethereum"            => Some(Chain.Ethereum)
    case "solana"              => Some(Chain.Solana)
    case "elrond"              => Some(Chain.Elrond)
    case "binance-smart-chain" => Some(Chain.Binance)
    case "avalanche"           => Some(Chain.Avalanche)
    case "fantom"              => Some(Chain.Fantom)
    case "polygon-pos"         => Some(Chain.Polygon)
    case "harmony-shard-0"     => Some(Chain.Harmony)
    case "dogechain"           => Some(Chain.Dogecoin)
    case _                     => None
