package org.pico.unsigned

package object syntax {
  implicit class ByteOps_Ghg39HP(val self: Byte) extends AnyVal {
    def hex: String = (0 until 8).reverse.map(i => (self >> i) & 0x1).mkString("")

    def asUnsigned: Nat8 = Nat8(self)
  }

  implicit class ShortOps_Ghg39HP(val self: Short) extends AnyVal {
    def hex: String = (0 until 16).reverse.map(i => (self >> i) & 0x1).mkString("")

    def asUnsigned: Nat16 = Nat16(self)
  }

  implicit class IntOps_Ghg39HP(val self: Int) extends AnyVal {
    def hex: String = (0 until 32).reverse.map(i => (self >> i) & 0x1).mkString("")

    def asUnsigned: Nat32 = Nat32(self)
  }

  implicit class LongOps_Ghg39HP(val self: Long) extends AnyVal {
    def hex: String = (0L until 64L).reverse.map(i => (self >> i) & 0x1).mkString("")

    def asUnsigned: Nat64 = Nat64(self)
  }

  implicit class ArrayByteOps_xxx(val self: Array[Byte]) extends AnyVal {
    def asUnsigned: Array[Nat8] = self.asInstanceOf[Array[Nat8]]
  }

  implicit class ArrayUnsigned8Ops_xxx(val self: Array[Nat8]) extends AnyVal {
    def asUnsigned: Array[Byte] = self.asInstanceOf[Array[Byte]]
  }
}
