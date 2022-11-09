package com.bjoru.cryptosis.providers.decoders

import cats.syntax.foldable.given
import cats.syntax.traverse.given

import io.circe.*

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*

final case class PricedToken(
  token: Token,
  price: Price
)

final case class ZapperResult(
  address: Address,
  chain:   Chain,
  tokens:  Seq[PricedToken],
  apps:    Seq[Defi]
)

object ZapperDecoders:

  given Decoder[PricedToken] = Decoder.instance { hc =>
    for key <- hc.downField("key").as[String]
        lbl <- hc.downField("displayProps").downField("label").as[String]
        con <- hc.downField("address").as[Option[Address]]
        net <- hc.downField("network").as[Chain]
        sym <- hc.downField("context").downField("symbol").as[Symbol]
        dec <- hc.downField("context").downField("decimals").as[Int]
        bal <- hc.downField("context").downField("balance").as[Balance]
        usd <- hc.downField("context").downField("price").as[Price]
    yield PricedToken(Token(key, lbl, sym, net, con, dec, bal), usd)
  }

  given Decoder[Defi] = Decoder.instance { hc =>
    for key <- hc.downField("key").as[String]
        aid <- hc.downField("appId").as[String]
        net <- hc.downField("network").as[Chain]
        lbl <- hc.downField("displayProps").downField("label").as[String]
        dap <- hc.downField("breakdown").as[Seq[Json]].flatMap(breakdowns(key, aid, net, _))
    yield dap
  }

  given Decoder[ZapperResult] = Decoder.instance { hc =>
    for addr  <- hc.downField("addresses").as[Seq[Address]].map(_.head)
        netw  <- hc.downField("network").as[Chain]
        tnode <- hc.downField("balance").downField("wallet").as[Json]
        dnode <- hc.downField("app").downField("data").as[Option[Seq[Json]]]

        // Tokens
        tkeys  = tnode.hcursor.keys.getOrElse(Iterable.empty)
        toks  <- tkeys.toSeq.traverse(k => tnode.hcursor.downField(k).as[PricedToken])

        // DApps
        dapps <- dnode.getOrElse(Seq.empty).traverse(_.as[Defi])
    yield ZapperResult(addr, netw, toks, dapps)
  }

  /////////////
  // Helpers //
  /////////////

  private def breakdowns(id: String, name: String, chain: Chain, data: Seq[Json]): Decoder.Result[Defi] =
    val items = data.traverse(breakdown).map(_.groupMap(_._1)(_._2))

    items.map(_.mapValues(_.flatten)).map {
      case e if e.contains("claimable") => Defi.Farm(
          providerId = id,
          name       = name,
          chain      = chain,
          liquidity  = e.get("supplied").getOrElse(Seq.empty),
          claimable  = e.get("claimable").getOrElse(Seq.empty)
        )
      case e => Defi.Stake(
          providerId = id,
          name       = name,
          chain      = chain,
          liquidity  = e.get("supplied").getOrElse(Seq.empty)
        )
    }

  private def breakdown(json: Json): Decoder.Result[(String, Seq[Token])] = 
    val ident = for a <- json.hcursor.downField("contractType").as[Option[String]]
                    b <- json.hcursor.downField("metaType").as[Option[String]]
                yield a -> b

    ident.flatMap {
      case (Some("add-token"), _) =>
        json.hcursor.downField("breakdown").as[Seq[PricedToken]].map("supplied" -> _.map(_.token))
      case (Some("base-token"), Some("claimable")) =>
        json.as[PricedToken].map(v => "claimable" -> Seq(v.token))
      case (Some("base-token"), _) =>
        json.as[PricedToken].map(v => "supplied" -> Seq(v.token))
      case _ => Right("unknown" -> Seq.empty)
    }
