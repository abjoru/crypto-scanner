package com.bjoru.cryptos.syntax

import com.bjoru.cryptos.types.*

trait Identity[T]:
  extension (t: T) def id: Id
