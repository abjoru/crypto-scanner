package com.bjoru.cryptosis.providers

import cats.effect.IO
import cats.syntax.traverse.*
import cats.syntax.foldable.*

import io.circe.*
import io.circe.syntax.*

import org.http4s.*
import org.http4s.client.Client
import org.http4s.circe.*
import org.http4s.circe.CirceEntityDecoder.given

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*

class CovalentHQ(ep: Endpoint) extends ProviderApi("covalenthq"):

  val supportedChains = Seq(
    Chain.Ethereum,
    Chain.Binance,
    Chain.Avalanche,
    Chain.Fantom,
    Chain.Polygon,
    Chain.Harmony
  )

  def chainId(chain: Chain) = chain match
    case Chain.Ethereum  => Right("1")
    case Chain.Polygon   => Right("137")
    case Chain.Avalanche => Right("43114")
    case Chain.Fantom    => Right("250")
    case Chain.Harmony   => Right("1666600000")
    case Chain.Binance   => Right("56")
    case other           => Left(new Exception(s"Unsupported CovalentHQ chain: $other!"))

  def mkUri(w: Wallet) =
    for cid  <- chainId(w.chain).toIO
        apik <- ep.apiKey.toIO
        base  = ep.uri.toString
        addr  = w.address.toString
        raw   = s"$base/$cid/address/$addr/balances_v2/?quote-currency=USD&format=JSON&key=$apik"
        uri  <- Uri.fromString(raw).toIO
    yield uri

  protected def sync(wallets: Seq[Wallet])(using Client[IO]): IO[SyncResponse] =
    val res = wallets.filter(w => supportedChains.contains(w.chain)).traverse {
      case w if w.isMultichain => multichain(w)
      case w                   => balance(w)
    }

    res.map(CovalentResponse(_))

  def balance(wallet: Wallet)(using client: Client[IO]): IO[SyncData] =
    for uri <- mkUri(wallet)
        jsn <- client.expect[Json](uri)
    yield SyncData(wallet.address, "bal", Seq(jsn))

  def multichain(wallet: Wallet)(using Client[IO]): IO[SyncData] =
    supportedChains.filterNot(_ == Chain.Solana)
                   .traverse(ch => balance(wallet.withChain(ch)).map(_.withKey(ch.toString)))
                   .map(_.reduce(_ + _))

class CovalentResponse(val data: Seq[SyncData]) extends FoldableSyncResponse:

  given Decoder[(Token, Option[Price])] = Decoder.instance { hc =>
    for a <- hc.downField("contract_name").as[String]
        b <- hc.downField("contract_ticker_symbol").as[Symbol]
        c <- hc.downField("contract_decimals").as[Int]
        d <- hc.downField("contract_address").as[Option[Address]]
        e <- hc.downField("balance").as[BigInt]
        f <- hc.downField("quote_rate").as[Option[Price]]
        r <- Balance.convert(c, e).toCirce(hc)
    yield Token(a, a, b, Chain.Unknown, d, c, r) -> f
  }

  def withData(extras: Seq[SyncData]): SyncResponse = CovalentResponse(data ++ extras)

  def syncWallet(env: Env, wallet: Wallet)(using Client[IO]) =
    case SyncData(_, "bal", Seq(json)) =>
      for items <- IO.pure(json.hcursor.downField("data").downField("items").values.getOrElse(Iterable.empty))
          toks  <- items.toSeq.traverse(_.as[(Token, Option[Price])]).toIO
          regs  <- env.registerWithPrice(toks)
      yield regs.tuple(tx => wallet.addBalances(tx: _*))

    case SyncData(_, chKey, Seq(json)) =>
      for chain <- Chain.fromString(chKey).toIO
          items  = json.hcursor.downField("data").downField("items").values.getOrElse(Iterable.empty)
          toks  <- items.toSeq.traverse(_.as[(Token, Option[Price])]).toIO
          toks2  = toks.map(v => (v._1.withChain(chain), v._2))
          regs  <- env.registerWithPrice(toks2)
      yield regs.tuple(tx => wallet.addBalances(tx: _*))
