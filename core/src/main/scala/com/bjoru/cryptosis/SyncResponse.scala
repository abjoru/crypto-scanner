package com.bjoru.cryptosis

import cats.effect.IO
import cats.syntax.foldable.*

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

  val data: Seq[SyncData]

  /** Synchronize wallets with this response.
    * This function will convert response data to structured
    * token/defi types and register them with the environment
    * before updating the wallets.
    *
    * @param wallets list of wallets to sync.
    * @return StateT of env and updated wallets
    */
  def syncWallets(wallets: Seq[Wallet])(env: Env)(using Client[IO]): IO[(Env, Seq[Wallet])]

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

  type Response[T] = PartialFunction[SyncData, IO[T]]

  final def syncWallets(wallets: Seq[Wallet])(env: Env)(using Client[IO]): IO[(Env, Seq[Wallet])] =
    wallets.foldLeftM(env -> Seq.empty[Wallet]) {
      case ((env2, updWallets), wallet) => intSyncWallet(env2, wallet).map {
        case (env3, updWallet) => (env3, updWallets :+ updWallet)
      }
    }

  private def intSyncWallet(env: Env, wallet: Wallet)(using Client[IO]): IO[(Env, Wallet)] = 
    val matchingData = data.filter(_.walletAddress == wallet.address)

    if matchingData.isEmpty
      then IO.pure(env -> wallet)
      else matchingData.foldLeftM(env -> wallet)((a, b) => intProcessData(a._1, a._2, b))

  private def intProcessData(env: Env, wallet: Wallet, item: SyncData)(using Client[IO]): IO[(Env, Wallet)] =
    val func = syncWallet(env, wallet).orElse(_ => IO.pure(env -> wallet))
    func(item)

  def syncWallet(env: Env, wallet: Wallet)(using Client[IO]): Response[(Env, Wallet)]
