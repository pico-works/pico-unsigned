package org.pico.unsigned

case class Nat32(asSigned: Int) extends AnyVal {
  @inline final def +(that: Nat32): Nat32 = Nat32(this.asSigned + that.asSigned)

  @inline final def -(that: Nat32): Nat32 = Nat32(this.asSigned - that.asSigned)

  @inline final def truncate8: Nat8 = Nat8(asSigned.toByte)

  @inline final def truncate16: Nat16 = Nat16(asSigned.toShort)

  @inline final def truncate32: Nat32 = this

  @inline final def truncate64: Nat64 = Nat64(asSigned.toLong & 0xffffL)

  @inline final def widen32: Nat32 = truncate32

  @inline final def widen64: Nat64 = truncate64

  @inline final def >>(offset: Long): Nat32 = {
    offset match {
      case o if o >= 32 => Nat32(0)
      case o if o > 0   => Nat32(asSigned >>> offset)
      case o if o > -32 => Nat32(asSigned << -offset)
      case _            => Nat32(0)
    }
  }

  @inline final def <<(offset: Long): Nat32 = {
    offset match {
      case o if o >= 32 => Nat32(0)
      case o if o > 0   => Nat32(asSigned <<   offset)
      case o if o > -32 => Nat32(asSigned >>> -offset)
      case _            => Nat32(0)
    }
  }

  @inline final def |(that: Nat32): Nat32 = Nat32(this.asSigned | that.asSigned)

  @inline final def &(that: Nat32): Nat32 = Nat32(this.asSigned & that.asSigned)

  @inline final def ^(that: Nat32): Nat32 = Nat32(this.asSigned ^ that.asSigned)

  @inline final def ##(that: Nat32): Nat64 = this.widen64 << 32 | that.widen64
}
