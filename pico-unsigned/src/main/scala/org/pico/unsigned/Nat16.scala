package org.pico.unsigned

case class Nat16(asSigned: Short) extends AnyVal {
  @inline final def +(that: Nat16): Nat16 = Nat16((this.asSigned + that.asSigned).toShort)

  @inline final def -(that: Nat16): Nat16 = Nat16((this.asSigned - that.asSigned).toShort)

  @inline final def truncate8: Nat8 = Nat8(asSigned.toByte)

  @inline final def truncate16: Nat16 = this

  @inline final def truncate32: Nat32 = Nat32(asSigned.toInt & 0xffff)

  @inline final def truncate64: Nat64 = Nat64(asSigned.toLong & 0xffffL)

  @inline final def widen16: Nat16 = truncate16

  @inline final def widen32: Nat32 = truncate32

  @inline final def widen64: Nat64 = truncate64

  @inline final def >>(offset: Byte): Nat16 = (widen32 >> offset).truncate16

  @inline final def <<(offset: Byte): Nat16 = (widen32 << offset).truncate16

  @inline final def |(that: Nat16): Nat16 = Nat16((this.asSigned | that.asSigned).toShort)

  @inline final def &(that: Nat16): Nat16 = Nat16((this.asSigned & that.asSigned).toShort)

  @inline final def ^(that: Nat16): Nat16 = Nat16((this.asSigned ^ that.asSigned).toShort)

  @inline final def ##(that: Nat16): Nat32 = this.widen32 << 16 | that.widen32
}
