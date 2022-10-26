package com.bjoru.cryptosis.types

import cats.effect.IO
import org.http4s.Uri

import pureconfig.ConfigReader
import pureconfig.generic.derivation.default.*

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.instances.given

final case class Endpoint(
  uri: Uri,
  apiKey: Option[String],
  secret: Option[String]
) derives ConfigReader
