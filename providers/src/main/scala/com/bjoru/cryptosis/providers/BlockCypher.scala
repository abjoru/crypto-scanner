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
    for _ <- putStrLn(f"$name%-15s: syncronizing wallets...")
        b <- wallets.filter(chainFilter).traverse(balanceOf).map(BlockChainResponse(_))
    yield b

  def balanceOf(wallet: Wallet)(using client: Client[IO]): IO[SyncData] =
    for url <- IO.pure(ep.uri / wallet.chain.symbol / "main" / "addrs" / wallet.address)
        jsn <- client.expect[Json](url)
    yield SyncData(wallet.address, "bal", Seq(jsn))

class BlockChainResponse(val data: Seq[SyncData]) extends FoldableSyncResponse:

  def withData(extras: Seq[SyncData]): SyncResponse = 
    BlockChainResponse(data ++ extras)

  def syncWallet(state: State, wallet: Wallet)(using Client[IO]) =
    case SyncData(_, "bal", Seq(json)) =>
      for rst <- IO.pure(state.bluechip(wallet.chain))
          num <- json.hcursor.downField("balance").as[BigInt].toIO
          bal <- Balance.convert(rst._2.decimals, num).toIO
      yield rst._1 -> wallet.addBalances(rst._2.withBalance(bal))
