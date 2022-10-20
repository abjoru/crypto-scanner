package com.bjoru.cryptosis.types

import com.bjoru.cryptosis.*

trait DApp:

  val providerId: String

  val name: String

  val liquidity: Seq[(Token, Balance)]

  val claimable: Seq[(Token, Balance)]

object DApp:

  given Identity[DApp] with
    extension (d: DApp) def id = Id.create(d.providerId)
