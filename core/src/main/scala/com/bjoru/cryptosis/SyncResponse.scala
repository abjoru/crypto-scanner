package com.bjoru.cryptosis

import cats.Show
import cats.effect.IO
import cats.syntax.eq.*
import cats.syntax.show.*
import cats.syntax.foldable.*
import cats.syntax.traverse.*

import org.http4s.client.Client

import io.circe.*
import io.circe.syntax.*

import com.bjoru.cryptosis.types.*

final case class SyncData(
  walletAddress: Address,
  key:           String,
  responseData:  Seq[Json],
)

object SyncData:

  given Show[SyncData] = Show.show { d =>
    s"${d.walletAddress} ${d.key} ${d.responseData.size} json docs"
  }

  given Encoder[SyncData] = Encoder.instance { data =>
    Json.obj(
      "key"    -> data.key.asJson,
      "wallet" -> data.walletAddress.asJson, 
      "data"   -> data.responseData.asJson
    )
  }

  given Decoder[SyncData] = Decoder.instance { hc =>
    for wallet <- hc.downField("wallet").as[Address]
        key    <- hc.downField("key").as[String]
        data   <- hc.downField("data").as[Seq[Json]]
    yield SyncData(wallet, key, data)
  }

  extension (d: SyncData)
    def +(o: SyncData): SyncData = d.copy(
      responseData = d.responseData ++ o.responseData
    )

    def withKey(key: String): SyncData = d.copy(key = key)

trait SyncResponse:

  val provider: ProviderName

  val data: Seq[SyncData]

  /** Synchronize wallets with this response.
    * This function will convert response data to structured
    * token/defi types and register them with the environment
    * before updating the wallets.
    *
    * @param wallets list of wallets to sync.
    * @return StateT of env and updated wallets
    */
  def syncWallets(wallets: Seq[Wallet])(using Client[IO]): SIO[Seq[Wallet]]

  /** Makes a copy of this response with the extra data added.
    *
    * @param extras data to add
    * @return new response
    */
  def withData(extras: Seq[SyncData]): SyncResponse

/** A foldable version of SyncResponse allowing synchronization
  * to happen on the individual wallet basis.
  */
trait FoldableSyncResponse extends SyncResponse:

  type Response[T] = PartialFunction[SyncData, SIO[T]]

  final def syncWallets(wallets: Seq[Wallet])(using Client[IO]): SIO[Seq[Wallet]] = 
    for _ <- info("reading results...")
        r <- wallets.traverse(internalSync)
    yield r

  private def info(msg: String): SIO[Unit] =
    SIO.liftF(putStrLn(f"$provider%-15s: $msg"))

  private def internalSync(wallet: Wallet)(using Client[IO]): SIO[Wallet] =
    val matchingData = data.filter(_.walletAddress eqv wallet.address)

    if matchingData.isEmpty
      then SIO.pure(wallet)
      else processDataFor(wallet, matchingData)

  private def processDataFor(wallet: Wallet, data: Seq[SyncData])(using Client[IO]): SIO[Wallet] =
    val func = syncWallet(wallet).orElse { item => 
      info(s"WARNING: no handler for ${item.show}").map(_ => wallet)
    }

    data.foldLeftM(wallet)((acc, item) => func(item).map(acc.merge))

  def syncWallet(wallet: Wallet)(using Client[IO]): Response[Wallet]
