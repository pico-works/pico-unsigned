package org.pico.unsigned

import org.scalacheck.{Gen, Prop}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class Nat64Spec extends Specification with ScalaCheck {
  "Nat8" should {
    "support + operation" in {
      Prop.forAll(Gen.choose(0, 255)) { x =>
        Prop.forAll(Gen.choose(0, 255 - x)) { y =>
          Nat8(x.toByte) + Nat8(y.toByte) must_=== Nat8((x + y).toByte)
        }
      }
    }

    "support - operation" in {
      Prop.forAll(Gen.choose(0, 255)) { x =>
        Prop.forAll(Gen.choose(0, x)) { y =>
          Nat8(x.toByte) - Nat8(y.toByte) must_=== Nat8((x - y).toByte)
        }
      }
    }

    "support < operation" in {
      Prop.forAll(Gen.choose(0, 255)) { x =>
        Prop.forAll(Gen.choose(0, 255)) { y =>
          (Nat8(x.toByte) < Nat8(y.toByte)) must_=== (x < y)
        }
      }
    }

    "support > operation" in {
      Prop.forAll(Gen.choose(0, 255)) { x =>
        Prop.forAll(Gen.choose(0, 255)) { y =>
          (Nat8(x.toByte) > Nat8(y.toByte)) must_=== (x > y)
        }
      }
    }

    "support <= operation" in {
      Prop.forAll(Gen.choose(0, 255)) { x =>
        Prop.forAll(Gen.choose(0, 255)) { y =>
          (Nat8(x.toByte) <= Nat8(y.toByte)) must_=== (x <= y)
        }
      }
    }

    "support << operation" in {
      Prop.forAll(Gen.choose(0, 255)) { x =>
        Prop.forAll(Gen.choose(0, 31)) { y =>
          // TODO: Work for more y values
          (Nat8(x.toByte) << y) must_=== Nat8((x << y).toByte)
        }
      }
    }

    "support >> operation" in {
      Prop.forAll(Gen.choose(0, 255)) { x =>
        Prop.forAll(Gen.choose(0, 31)) { y =>
          // TODO: Work for more y values
          (Nat8(x.toByte) >> y) must_=== Nat8((x >> y).toByte)
        }
      }
    }

    "support truncate8 operation" in {
      Prop.forAll(Gen.choose(0, 255)) { x =>
        Nat8(x.toByte).truncate8 must_=== Nat8(x.toByte)
      }
    }

    "support truncate16 method that is equivalent to widen16 method" in {
      Prop.forAll(Gen.choose(0, 255)) { x =>
        Nat8(x.toByte).truncate16 must_=== Nat8(x.toByte).widen16
      }
    }

    "support truncate32 method that is equivalent to widen32 method" in {
      Prop.forAll(Gen.choose(0, 255)) { x =>
        Nat8(x.toByte).truncate32 must_=== Nat8(x.toByte).widen32
      }
    }

    "support truncate64 method that is equivalent to widen64 method" in {
      Prop.forAll(Gen.choose(0, 255)) { x =>
        Nat8(x.toByte).truncate64 must_=== Nat8(x.toByte).widen64
      }
    }

    "support truncate64 method that is equivalent to widen64 method" in {
      Prop.forAll(Gen.choose(0, 255)) { x =>
        Nat8(x.toByte).truncate64 must_=== Nat8(x.toByte).widen64
      }
    }
  }
}
