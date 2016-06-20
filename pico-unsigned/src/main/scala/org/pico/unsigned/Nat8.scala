package org.pico.unsigned

case class Nat8(asSigned: Byte) extends AnyVal {
  @inline final def +(that: Nat8): Nat8 = Nat8((this.asSigned + that.asSigned).toByte)

  @inline final def -(that: Nat8): Nat8 = Nat8((this.asSigned - that.asSigned).toByte)

  @inline final def <(that: Nat8): Boolean = (this.asSigned & 0xff) < (that.asSigned & 0xff)

  @inline final def >(that: Nat8): Boolean = (this.asSigned & 0xff) > (that.asSigned & 0xff)

  @inline final def <=(that: Nat8): Boolean = (this.asSigned & 0xff) <= (that.asSigned & 0xff)

  @inline final def >=(that: Nat8): Boolean = (this.asSigned & 0xff) >= (that.asSigned & 0xff)

  @inline final def truncate8: Nat8 = this

  @inline final def truncate16: Nat16 = Nat16((asSigned.toInt & 0xff).toShort)

  @inline final def truncate32: Nat32 = Nat32(asSigned.toInt & 0xff)

  @inline final def truncate64: Nat64 = Nat64(asSigned.toLong & 0xffL)

  @inline final def widen8: Nat8 = truncate8

  @inline final def widen16: Nat16 = truncate16

  @inline final def widen32: Nat32 = truncate32

  @inline final def widen64: Nat64 = truncate64

  @inline final def >>(offset: Int): Nat8 = (widen32 >> offset).truncate8

  @inline final def <<(offset: Int): Nat8 = (widen32 << offset).truncate8

  @inline final def |(that: Nat8): Nat8 = Nat8((this.asSigned | that.asSigned).toByte)

  @inline final def &(that: Nat8): Nat8 = Nat8((this.asSigned & that.asSigned).toByte)

  @inline final def ^(that: Nat8): Nat8 = Nat8((this.asSigned ^ that.asSigned).toByte)

  @inline final def ##(that: Nat8): Nat16 = this.widen16 << 8 | that.widen16
}
