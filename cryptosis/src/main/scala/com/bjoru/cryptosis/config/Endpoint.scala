package com.bjoru.cryptosis.config

import cats.effect.IO
import org.http4s.Uri

import pureconfig.ConfigReader
import pureconfig.generic.derivation.default.*

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.instances.given

final case class Endpoint(
  uri:    Uri,
  apiKey: String,
  secret: Option[String]
) derives ConfigReader

object Endpoint:

  def loadEndpoints(file: FilePath): IO[Map[Provider, Endpoint]] =
    loadYamlFile[Map[Provider, Endpoint]](file)

  def loadExchanges(file: FilePath): IO[Map[ExchangeName, Endpoint]] =
    loadYamlFile[Map[ExchangeName, Endpoint]](file)

  def findEndpoint(file: FilePath, provider: Provider): IO[Endpoint] =
    for all <- loadEndpoints(file)
        res <- IO.fromOption(all.get(provider))(new Exception(s"Missing $provider!"))
    yield res

  def findExchange(file: FilePath, name: ExchangeName): IO[Endpoint] =
    for all <- loadExchanges(file)
        res <- IO.fromOption(all.get(name))(Exception(s"Missing $name!"))
    yield res
