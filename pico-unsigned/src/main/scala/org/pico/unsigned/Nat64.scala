package org.pico.unsigned

case class Nat64(asSigned: Long) extends AnyVal {
  @inline final def +(that: Nat64): Nat64 = Nat64(this.asSigned + that.asSigned)

  @inline final def -(that: Nat64): Nat64 = Nat64(this.asSigned - that.asSigned)

  @inline final def <(that: Nat64): Boolean = (this.asSigned + Long.MinValue) < (that.asSigned + Long.MinValue)

  @inline final def >(that: Nat64): Boolean = (this.asSigned + Long.MinValue) > (that.asSigned + Long.MinValue)

  @inline final def <=(that: Nat64): Boolean = (this.asSigned + Long.MinValue) <= (that.asSigned + Long.MinValue)

  @inline final def >=(that: Nat64): Boolean = (this.asSigned + Long.MinValue) >= (that.asSigned + Long.MinValue)

  @inline final def truncate8: Nat8 = Nat8(asSigned.toByte)

  @inline final def truncate16: Nat16 = Nat16(asSigned.toShort)

  @inline final def truncate32: Nat32 = Nat32(asSigned.toInt)

  @inline final def truncate64: Nat64 = this

  @inline final def widen64: Nat64 = truncate64

  @inline final def >>(offset: Long): Nat64 = {
    offset match {
      case o if o >= 64 => Nat64(0L)
      case o if o > 0   => Nat64(asSigned >>> offset)
      case o if o > -64 => Nat64(asSigned << -offset)
      case _            => Nat64(0L)
    }
  }

  @inline final def <<(offset: Long): Nat64 = {
    offset match {
      case o if o >= 64 => Nat64(0L)
      case o if o > 0   => Nat64(asSigned <<   offset)
      case o if o > -64 => Nat64(asSigned >>> -offset)
      case _            => Nat64(0L)
    }
  }

  @inline final def |(that: Nat64): Nat64 = Nat64(this.asSigned | that.asSigned)

  @inline final def &(that: Nat64): Nat64 = Nat64(this.asSigned & that.asSigned)

  @inline final def ^(that: Nat64): Nat64 = Nat64(this.asSigned ^ that.asSigned)
}
