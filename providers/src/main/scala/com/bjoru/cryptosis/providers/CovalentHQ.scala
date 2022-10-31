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
    for ws <- IO.pure(wallets.filter(w => supportedChains.contains(w.chain)))
        _  <- putStrLn(f"$name%-15s: synchronizing wallets...")
        rs <- ws.foldLeftM(Seq.empty[SyncData])((a, b) => syncWallet(b).map(_ ++ a))
    yield CovalentResponse(rs)

  protected def syncWallet(w: Wallet)(using Client[IO]): IO[Seq[SyncData]] = 
    if w.isMultichain
      then multichain(w)
      else balance(w).map(Seq(_))

  def balance(wallet: Wallet)(using client: Client[IO]): IO[SyncData] =
    for uri <- mkUri(wallet)
        jsn <- client.expect[Json](uri)
    yield SyncData(wallet.address, "bal", Seq(jsn))

  def multichain(wallet: Wallet)(using Client[IO]): IO[Seq[SyncData]] =
    supportedChains.filterNot(_ == Chain.Solana)
                   .traverse(ch => balance(wallet.withChain(ch)).map(_.withKey(ch.toString)))

class CovalentResponse(val data: Seq[SyncData]) extends FoldableSyncResponse:

  given Decoder[(Token, Option[Price])] = Decoder.instance { hc =>
    for a <- hc.downField("contract_name").as[Option[String]]
        b <- hc.downField("contract_ticker_symbol").as[Symbol]
        c <- hc.downField("contract_decimals").as[Int]
        d <- hc.downField("contract_address").as[Option[Address]]
        e <- hc.downField("balance").as[BigInt]
        f <- hc.downField("quote_rate").as[Option[Price]]
        r <- Balance.convert(c, e).toCirce(hc)
    yield Token(a.getOrElse(b.lower), a.getOrElse(b.lower), b, Chain.Unknown, d, c, r) -> f
  }

  def withData(extras: Seq[SyncData]): SyncResponse = CovalentResponse(data ++ extras)

  def syncWallet(state: State, wallet: Wallet)(using Client[IO]) =
    case SyncData(_, "bal", Seq(json)) =>
      for items <- IO.pure(json.hcursor.downField("data").downField("items").values.getOrElse(Iterable.empty))
          toks  <- items.toSeq.traverse(_.as[(Token, Option[Price])]).toIO
          regs   = state.resolveAllWithPrice(toks.map(v => v._1 -> v._2.getOrElse(Price.Zero)))
      yield regs._1 -> wallet.addBalances(regs._2: _*)

    case SyncData(_, chKey, jsons) =>
      jsons.foldLeftM(state -> wallet) {
        case ((st2, w), json) =>
          for chain <- Chain.fromString(chKey).toIO
              items  = json.hcursor.downField("data").downField("items").values.getOrElse(Iterable.empty)
              jsns   = items.filterNot(reject)
              toks1 <- jsns.toSeq.traverse(_.as[(Token, Option[Price])]).toIO
              toks2  = toks1.map(v => (v._1.withChain(chain), v._2.getOrElse(Price.Zero)))
              regs   = st2.resolveAllWithPrice(toks2)
          yield regs._1 -> w.addBalances(regs._2: _*)
      }

  def reject(json: Json): Boolean = Seq(
    json.hcursor.downField("contract_ticker_symbol").focus.map(_.isNull).getOrElse(false),
    json.hcursor.downField("type").as[String].map(_ == "dust").getOrElse(false)
  ).contains(true)
