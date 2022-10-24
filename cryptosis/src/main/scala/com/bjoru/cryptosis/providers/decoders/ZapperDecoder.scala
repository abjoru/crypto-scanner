package com.bjoru.cryptosis.providers.decoders

import cats.effect.IO
import cats.syntax.foldable.given
import cats.syntax.traverse.given

import io.circe.*

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

  def decodeMulti(data: Seq[Json], env: Env): IO[(Env, Seq[Defi])] =
    data.foldLeftM(env -> Seq.empty[Defi]) {
      case ((e2, acc), j) => decodeApps(j, e2).map(v => v._1 -> (acc ++ v._2))
    }

  def decodeDefiApps(env: Env, apps: Map[Wallet, Seq[Json]]): IO[(Env, Seq[Wallet])] = 
    apps.toSeq.foldLeftM(env -> Seq.empty[Wallet]) {
      case ((e2, acc), (w, js)) =>
        decodeMulti(js, e2).map(v => v._1 -> (acc :+ w.addBalances(v._2)))
    }

  def decodeTokenApps(env: Env, apps: Map[Wallet, Seq[Json]]): IO[(Env, Seq[Wallet])] =
    apps.toSeq.foldLeftM(env -> Seq.empty[Wallet]) {
      case ((e2, acc), (w, js)) =>
        val updWallet = js.foldLeftM(e2 -> w) {
          case ((e3, w2), j) =>
            for node <- (j <\> "balance" <\> "wallet").asIO[Json]
                keys  = node.hcursor.keys.getOrElse(Iterable.empty)
                toks <- keys.toSeq.traverse(k => node.hcursor.downField(k).asIO[Token])
                res   = e3.resolveAndUpdateAll(toks)
            yield res._1 -> w2.addBalances(res._2)
        }

        updWallet.map(v => v._1 -> (acc :+ v._2))
    }

  private def decodeApps(data: Json, env: Env): IO[(Env, Seq[Defi])] = 
    for addr <- (data <\> "addresses").asIO[Seq[String]].map(_.head)
        itms <- (data <\> "app" <\> "data").asIO[Option[Seq[Json]]].map(_.getOrElse(Seq.empty))
        res  <- buildApps(env, itms)
    yield res

  private def buildApps(env: Env, data: Seq[Json]): IO[(Env, Seq[Defi])] =
    data.foldLeftM(env -> Seq.empty[Defi]) {
      case ((e2, acc), d) => decodeApp(e2, d).map(v => v._1 -> (acc :+ v._2))
    }

  private def decodeApp(env: Env, data: Json): IO[(Env, Defi)] =
    for key <- (data <\> "key").asIO[String]
        aid <- (data <\> "appId").asIO[String]
        chi <- (data <\> "network").asIO[Chain]
        lbl <- (data <\> "displayProps" <\> "label").asIO[String]
        brd <- (data <\> "breakdown").asIO[Seq[Json]].flatMap(decodeAppBreakdowns(key, lbl, chi, env, _))
    yield brd

  private def decodeAppBreakdowns(
    id: String, 
    name: String, 
    chain: Chain, 
    env: Env, 
    data: Seq[Json]
  ): IO[(Env, Defi)] =
    val results = data.foldLeftM(env -> Map.empty[String, Seq[Token]]) {
      case ((e2, acc), json) => decodeAppBreakdown(e2, json).map {
        case ((e3, (id, tx))) => e3 -> acc.updatedWith(id) {
          case Some(xs) => Some(xs ++ tx)
          case None     => Some(tx)
        }
      }
    }

    results.map {
      case (e, items) if items.contains("claimable") => e -> Defi.Farm(
        providerId = id, 
        name = name, 
        chain = chain, 
        liquidity = items.get("supplied").getOrElse(Seq.empty),
        claimable = items.get("claimable").getOrElse(Seq.empty)
      )

      case (e, items) => e -> Defi.Stake(
        providerId = id,
        name = name,
        chain = chain,
        liquidity = items.get("supplied").getOrElse(Seq.empty)
      )
    }

  private def decodeAppBreakdown(env: Env, data: Json): IO[(Env, (String, Seq[Token]))] =
    val ident = for a <- (data <\> "contractType").asIO[Option[String]]
                    b <- (data <\> "metaType").asIO[Option[String]]
                yield a -> b

    ident.flatMap {
      case (Some("app-token"), _) => 
        val tokens = (data <\> "breakdown").asIO[Seq[Token]]
        val result = tokens.map(env.resolveAndUpdateAll(_))
        result.map(v => v._1 -> ("supplied" -> v._2))

      case (Some("base-token"), Some("claimable")) =>
        val token = data.asIO[Token].map(env.resolveAndUpdate(_))
        token.map(v => v._1 -> ("claimable" -> Seq(v._2)))

      case (Some("base-token"), _) =>
        val token = data.asIO[Token].map(env.resolveAndUpdate(_))
        token.map(v => v._1 -> ("supplied" -> Seq(v._2)))


      case _ => IO.pure(env -> ("unknown" -> Seq.empty[Token]))
    }
