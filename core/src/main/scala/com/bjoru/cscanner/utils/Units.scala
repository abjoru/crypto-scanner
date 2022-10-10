package com.bjoru.cscanner.utils

import java.math.BigDecimal

enum Unit(val name: String, val factor: Int):
  case Wei extends Unit("wei", 0)
  case KWei extends Unit("kwei", 3)
  case MWei extends Unit("mwei", 6)
  case GWei extends Unit("gwei", 9)
  case Szabo extends Unit("szabo", 12)
  case Finney extends Unit("finney", 15)
  case Ether extends Unit("ether", 18)
  case KEther extends Unit("kether", 21)
  case MEther extends Unit("mether", 24)
  case GEther extends Unit("gether", 27)

extension (u: Unit)
  def weiFactor = BigDecimal.TEN.pow(u.factor)

def fromWei(number: String, unit: Unit): BigDecimal =
  fromWei(new BigDecimal(number), unit)

def fromWei(number: BigInt, unit: Unit): BigDecimal =
  fromWei(BigDecimal(number.underlying), unit)

def fromWei(number: BigDecimal, unit: Unit): BigDecimal =
  number.divide(unit.weiFactor)
