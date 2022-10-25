package com.bjoru.cryptosis.providers

import cats.syntax.traverse.given
import cats.syntax.foldable.given
import cats.effect.IO

import io.circe.*
import io.circe.syntax.{*, given}

import org.http4s.Uri
import org.http4s.dsl.io.*
import org.http4s.client.*
import org.http4s.client.dsl.io.*
import org.http4s.circe.*
import org.http4s.circe.CirceEntityDecoder.given

import scala.util.{Try, Success, Failure}

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*

class CovalentHQ(ep: Endpoint, filters: Seq[TokenFilter]) extends ProviderApi:

  given Decoder[Token] = Decoder.instance { hc =>
    for a <- hc.downField("contract_name").as[String]
        b <- hc.downField("contract_ticker_symbol").as[Symbol]
        c <- hc.downField("contract_decimals").as[Int]
        d <- hc.downField("contract_address").as[Address]
        e <- hc.downField("balance").as[BigInt]
        f <- hc.downField("quote_rate").as[Price]
    yield Token(a, a, b, Chain.Unknown, c, d, Balance.fromRaw(e, c), f)
  }

  val supportedChains = Seq(
    Chain.Ethereum,
    Chain.Binance,
    Chain.Avalanche,
    Chain.Fantom,
    Chain.Polygon,
    Chain.Harmony
  )

  protected def doSync(wallets: Seq[Wallet], env: Env)(using client: Client[IO]): IO[(Env, Seq[Wallet])] =
    foreachWallet(env, wallets) {
      case (e, w) if w.isMultichain => multichain(w, client, e)
      case (e, w)                   => balance(w, client, e)
    }

  private def doSyncWallets(env: Env, wallets: Seq[Wallet])(using client: Client[IO]): IO[(Env, Seq[Wallet])] =
    wallets.foldLeftM(env -> Seq.empty[Wallet]) {
      case ((e, acc), w) if w.isMultichain => 
        multichain(w, client, e).map(kv => kv._1 -> (acc :+ kv._2))
      case ((e, acc), w) =>
        balance(w, client, e).map(kv => kv._1 -> (acc :+ kv._2))
    }

  private def chainId(chain: Chain) = chain match
    case Chain.Ethereum  => Right("1")
    case Chain.Polygon   => Right("137")
    case Chain.Avalanche => Right("43114")
    case Chain.Fantom    => Right("250")
    case Chain.Harmony   => Right("1666600000")
    case Chain.Binance   => Right("56")
    case other           => Left(new Exception(s"Unsupported CovalentHQ chain: $other!"))

  private def filter(c: Chain, tx: Seq[Token]) =
    filters.find(_.chain == c).map(_.filterTokens(tx)).getOrElse(tx)

  private def balanceUri(w: Wallet) =
    for cid  <- IO.fromEither(chainId(w.chain))
        base  = ep.uri.toString
        addr  = w.address.str
        apik  = ep.apiKey
        form  = s"$base/$cid/address/$addr/balances_v2/?quote-currency=USD&format=JSON&key=$apik"
        uri  <- IO.fromEither(Uri.fromString(form))
    yield uri

  private def balance(w: Wallet)(using cl: Client[IO]): IO[Wallet] =
    for a <- balanceUri(w)
        b <- cl.expect(a)(jsonOf[IO, Json])
        c  = b.hcursor.downField("data").downField("items").values.map(decode).getOrElse(Iterable.empty)
        d  = env.resolveAndUpdateAll(filter(w.chain, c.toSeq).map(_.withChain(w.chain)))
    yield d._1 -> w.addBalances(d._2)

  private def multichain(w: Wallet)(using Client[IO]): IO[Wallet] =
    val chains = supportedChains.filterNot(_ == Chain.Solana)

    chains.foldLeftM(env -> w) {
      case ((e, bw), ch) =>
        val res = balance(w.withChain(ch), cl, e)
        res.map(kv => kv._1 -> bw.merge(kv._2))
    }

  private def decode(jx: Iterable[Json]): Iterable[Token] =
    jx.map(_.as[Token].toOption).flatten
