package com.bjoru.cryptosis.types

import com.bjoru.cryptosis.*

final case class Token(
  name:     String,
  symbol:   Symbol,
  chain:    Chain,
  contract: Option[Address]
)

object Token:

  given Identity[Token] with
    extension (t: Token) def id = t match
      case Token(_, s, c, Some(a)) => Id.create(s.lower, c.str, a.str)
      case Token(_, s, c, _)       => Id.create(s.lower, c.str)
