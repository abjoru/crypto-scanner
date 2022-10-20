package com.bjoru.cryptosis.config

import cats.effect.IO
import org.http4s.Uri

import pureconfig.ConfigReader
import pureconfig.generic.derivation.default.*

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.instances.given

final case class Endpoint(
  uri:    Uri,
  apiKey: String
) derives ConfigReader

object Endpoint:

  def loadEndpoints(file: FilePath): IO[Map[Provider, Endpoint]] =
    loadYamlFile[Map[Provider, Endpoint]](file)

  def findEndpoint(file: FilePath, provider: Provider): IO[Endpoint] =
    for all <- loadEndpoints(file)
        res <- IO.fromOption(all.get(provider))(new Exception(s"Missing $provider!"))
    yield res
