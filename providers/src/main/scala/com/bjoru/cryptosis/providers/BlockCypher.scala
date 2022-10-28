package com.bjoru.cryptosis.providers

import cats.effect.IO
import cats.syntax.traverse.given
import cats.syntax.foldable.given

import io.circe.*
import io.circe.syntax.*

import org.http4s.client.*
import org.http4s.implicits.uri
import org.http4s.circe.*
import org.http4s.circe.CirceEntityDecoder.given

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*

class BlockCypher(ep: Endpoint) extends ProviderApi("blockcypher"):

  val chainFilter: Wallet => Boolean = _.chain match
    case Chain.Bitcoin  => true
    case Chain.Dogecoin => true
    case _              => false

  protected def sync(wallets: Seq[Wallet])(using Client[IO]): IO[SyncResponse] =
    wallets.filter(chainFilter).traverse(balanceOf).map(BlockChainResponse(_))

  def balanceOf(wallet: Wallet)(using client: Client[IO]): IO[SyncData] =
    for url <- IO.pure(ep.uri / wallet.chain.symbol / "main" / "addrs" / wallet.address)
        jsn <- client.expect[Json](url)
    yield SyncData(wallet.address, "bal", Seq(jsn))

class BlockChainResponse(val data: Seq[SyncData]) extends SyncResponse:

  def syncWallets(wallets: Seq[Wallet])(env: Env)(using Client[IO]): IO[(Env, Seq[Wallet])] = 
    wallets.foldLeftM(env -> Seq.empty[Wallet]) {
      case ((e2, wx), w) => parseSyncData(e2)(w).map(v => v._1 -> (wx :+ v._2))
    }

  def withData(extras: Seq[SyncData]): SyncResponse = 
    BlockChainResponse(data ++ extras)

  private def parseSyncData(env: Env)(wallet: Wallet)(using Client[IO]): IO[(Env, Wallet)] =
    data.find(_.walletAddress == wallet.address) match
      case Some(SyncData(_, "bal", Seq(json))) =>
        for token <- env.bluechip(wallet.chain)
            env2  <- env.register(token)
            tok2   = env2._2.collect { case t: Token => t }.head
            res   <- IO.fromEither(json.hcursor.downField("balance").as[BigInt])
            bal   <- IO.fromTry(Balance.convert(tok2.decimals, res))
        yield env2._1 -> wallet.addBalances(tok2.withBalance(bal))
      case _ => IO.pure(env -> wallet)
