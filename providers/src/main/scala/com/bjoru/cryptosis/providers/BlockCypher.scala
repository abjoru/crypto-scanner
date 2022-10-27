package com.bjoru.cryptosis.providers

import cats.effect.IO
import cats.syntax.traverse.given

import io.circe.*
import io.circe.syntax.*

import org.http4s.client.*
import org.http4s.implicits.uri
import org.http4s.circe.*
import org.http4s.circe.CirceEntityDecoder.given

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*

class BlockCypher(ep: Endpoint) extends ProviderApi:

  def sync(wallets: Seq[Wallet])(using Client[IO]): IO[SyncResponse] =
    wallets.traverse(balanceOf).map(BlockChainResponse(_))

  def balanceOf(wallet: Wallet)(using client: Client[IO]): IO[SyncData] =
    for url <- IO.pure(ep.uri / wallet.chain.symbol / "main" / "addrs" / wallet.address)
        jsn <- client.expect[Json](url)
    yield SyncData(wallet.address, Seq(jsn))

class BlockChainResponse(val data: Seq[SyncData]) extends SyncResponse:

  def syncWallets(wallets: Seq[Wallet]): SIO[Seq[Wallet]] = 
    SIO.inspectF(env => wallets.traverse(parseSyncData(env)))

  private def parseSyncData(env: Env)(wallet: Wallet): IO[Wallet] =
    data.find(_.walletAddress == wallet.address) match
      case Some(SyncData(_, Seq(json))) =>
        for token <- IO.pure(env.bluechip(wallet.chain))
            res   <- IO.fromEither(json.hcursor.downField("balance").as[BigInt])
            bal   <- IO.fromTry(Balance.convert(token.decimals, res))
        yield wallet.addBalances(token.withBalance(bal))
      case _ => IO.pure(wallet)
