package com.bjoru.cryptosis

import cats.effect.IO

import org.http4s.client.*

import com.bjoru.cryptosis.types.*
import com.bjoru.cryptosis.config.*

trait ExchangeApi:

  def sync(env: Env, client: Client[IO]): IO[(Env, Exchange)]

object ExchangeApi:

  def loadApis(cfgDir: FilePath): IO[Seq[ExchangeApi]] =
    Endpoint.loadExchanges(cfgDir </> "exchanges.yaml").map(resolveApis)

  private def resolveApis(endpoints: Map[ExchangeName, Endpoint]): Seq[ExchangeApi] =
    val apis = endpoints.collect {
      case (ExchangeName.BinanceUS, e) => exchanges.BinanceUS(e)
    }

    apis.toSeq
