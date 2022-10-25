package com.bjoru.cryptosis.providers.decoders

import cats.effect.IO
import cats.syntax.foldable.given
import cats.syntax.traverse.given

import io.circe.*

import org.http4s.client.Client

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*
import com.bjoru.cryptosis.syntax.circe.*

object ZapperDecoder:

  given Decoder[Token] = Decoder.instance { data =>
    for key <- (data <\> "key").as[String]
        lbl <- (data <\> "displayProps" <\> "label").as[String]
        con <- (data <\> "address").as[Address]
        net <- (data <\> "network").as[Chain]
        sym <- (data <\> "context" <\> "symbol").as[Symbol]
        dec <- (data <\> "context" <\> "decimals").as[Int]
        bal <- (data <\> "context" <\> "balance").as[Balance]
        prs <- (data <\> "context" <\> "price").as[Price]
    yield Token(key, lbl, sym, net, dec, con, bal, prs)
  }

  def decodeDefiApps(apps: Map[Wallet, Seq[Json]])(using Client[IO]): IO[Seq[Wallet]] = 
    apps.toSeq.traverse {
      case (wallet, jsons) => jsons.foldLeftM(wallet) {
        case (acc, json) => decodeApps(json).map(acc.addBalances(_))
      }
    }

  def decodeTokenApps(apps: Map[Wallet, Seq[Json]])(using Client[IO]): IO[Seq[Wallet]] =
    apps.toSeq.traverse {
      case (wallet, jsons) => jsons.foldLeftM(wallet) { (w, json) =>
        for node <- (json <\> "balance" <\> "wallet").asIO[Json]
            keys  = node.hcursor.keys.getOrElse(Iterable.empty)
            toks <- keys.toSeq.traverse(k => (node <\> k).asIO[Token])
            res  <- Env.resolveTokens(toks)
        yield w.addBalances(res)
      }
    }

  private def decodeApps(data: Json)(using Client[IO]): IO[Seq[Defi]] = 
    for addr <- (data <\> "addresses").asIO[Seq[String]].map(_.head)
        itms <- (data <\> "app" <\> "data").asIO[Option[Seq[Json]]].map(_.getOrElse(Seq.empty))
        res  <- itms.traverse(decodeApp)
    yield res

  private def decodeApp(data: Json)(using Client[IO]): IO[Defi] =
    for key <- (data <\> "key").asIO[String]
        aid <- (data <\> "appId").asIO[String]
        chi <- (data <\> "network").asIO[Chain]
        lbl <- (data <\> "displayProps" <\> "label").asIO[String]
        brd <- (data <\> "breakdown").asIO[Seq[Json]].flatMap(decodeAppBreakdowns(key, lbl, chi, _))
    yield brd

  private def decodeAppBreakdowns(
    id: String, 
    name: String, 
    chain: Chain, 
    data: Seq[Json]
  )(using Client[IO]): IO[Defi] =
    data.traverse(decodeAppBreakdown).map(_.toMap).map {
      case e if e.contains("claimable") => Defi.Farm(
        providerId = id, 
        name = name, 
        chain = chain, 
        liquidity = e.get("supplied").getOrElse(Seq.empty),
        claimable = e.get("claimable").getOrElse(Seq.empty)
      )
      case e => Defi.Stake(
        providerId = id,
        name = name,
        chain = chain,
        liquidity = e.get("supplied").getOrElse(Seq.empty)
      )
    }

  private def decodeAppBreakdown(data: Json)(using Client[IO]): IO[(String, Seq[Token])] =
    val ident = for a <- (data <\> "contractType").asIO[Option[String]]
                    b <- (data <\> "metaType").asIO[Option[String]]
                yield a -> b

    ident.flatMap {
      case (Some("app-token"), _) => 
        val tokens = (data <\> "breakdown").asIO[Seq[Token]]
        val result = tokens.flatMap(Env.resolveTokens(_))
        result.map(v => ("supplied" -> v))

      case (Some("base-token"), Some("claimable")) =>
        val token = data.asIO[Token].flatMap(Env.resolveToken(_))
        token.map(v => "claimable" -> Seq(v))

      case (Some("base-token"), _) =>
        val token = data.asIO[Token].flatMap(Env.resolveToken(_))
        token.map(v => "supplied" -> Seq(v))


      case _ => IO.pure("unknown" -> Seq.empty[Token])
    }
