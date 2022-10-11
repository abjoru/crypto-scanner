package com.bjoru.cscanner

import java.nio.file.{Path, Paths}

import java.io.File

trait FilePath[T]:
  extension (t: T) def </>(o: String): Path

given FilePath[String] with
  extension (s: String) def </>(o: String) = Paths.get(s, o)

given FilePath[File] with
  extension (f: File) def </>(o: String) = f.toPath.resolve(o)

given FilePath[Path] with
  extension (p: Path) def </>(o: String) = p.resolve(o)

enum Xdg:
  case Data
  case Config
  case Cache
  case State

lazy val homeDir = System.getProperty("user.home")

final val TOKENS_FILE    = "tokens.yaml"
final val WALLETS_FILE   = "wallets.yaml"
final val ENDPOINTS_FILE = "endpoints.yaml"

def getXdgDirectory(xdg: Xdg): Path = xdg match
  case Xdg.Data   => Paths.get(homeDir, ".local", "share")
  case Xdg.Config => Paths.get(homeDir, ".config")
  case Xdg.Cache  => Paths.get(homeDir, ".cache")
  case Xdg.State  => Paths.get(homeDir, ".local", "state")

def getHomeDirectory: Path = Paths.get(homeDir)
