package com.bjoru.cryptosis.syntax

import cats.effect.IO

import scala.concurrent.duration.*
import scala.jdk.javaapi.DurationConverters.toJava

import java.nio.file.Path
import java.time.{LocalDateTime, Instant, ZoneId}

object PathOps:

  def delete(v: Path): IO[Boolean] = IO(v.toFile.delete())

  def mkdirs(v: Path): IO[Boolean] = v.toFile.exists() match
    case false if v.toFile.isDirectory => IO(v.toFile.mkdirs())
    case false                         => IO(v.toFile.getParentFile.mkdirs())
    case true                          => IO.pure(true) // already exist

  def expired(v: Path, duration: FiniteDuration): Boolean =
    v.toFile.exists() match
      case true if v.toFile.isFile =>
        val lm = Instant.ofEpochMilli(v.toFile.lastModified())
        val ts = LocalDateTime.ofInstant(lm, ZoneId.systemDefault)
        val fu = ts.plus(toJava(duration))
        fu.isBefore(LocalDateTime.now)
      case _ => true
