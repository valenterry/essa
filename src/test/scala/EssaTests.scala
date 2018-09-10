import essa.NestedGeneric
import essa.essa.convert
import shapeless._
import utest._

object EssaTests extends TestSuite {
  def success = assert(true)
  val tests = Tests {
    "Non-generic types" - {
      val testee = "hi"
      val result = convert[String](testee)
      assert(result == testee)
    }

    "Eempty case classes" - {
      case class A()
      case class B()

      val testee    = A()
      val result: B = convert[B](testee)
    }

    "Case classes with one field" - {
      case class A(a: String)
      case class B(a: String)

      val field     = "hi"
      val result: B = convert[B](A("hi"))
      assert(result == B(field))
    }

    "Case classes with multiple fields" - {
      case class A(a: String, b: Double)
      case class B(a: String, b: Double)

      val field1    = "hi"
      val field2    = 42.2
      val result: B = convert[B](A(field1, field2))
      assert(result == B(field1, field2))
    }

    "Case classes with empty sub case class" - {
      case class A1(a: A2)
      case class A2()

      case class B1(a: B2)
      case class B2()

      val result: B1 = convert[B1](A1(A2()))
      success
    }

    "Case classes with empty sub case classes and non-generic fields" - {
      case class A1(d: A2, c: Int, a: A2, b: String)
      case class A2()

      case class B1(d: B2, c: Int, a: B2, b: String)
      case class B2()

      val field1 = 42
      val field2 = "hi"

      val result: B1 = convert[B1](A1(A2(), field1, A2(), field2))
      assert(result == B1(B2(), field1, B2(), field2))
    }

    "Multi level nested case classes with empty sub case class at the end" - {
      case class A1(l2: A2)
      case class A2(l3: A3)
      case class A3()

      case class B1(l2: B2)
      case class B2(l3: B3)
      case class B3()

      val result: B1 = convert[B1](A1(A2(A3())))
      assert(result == B1(B2(B3())))
    }

    "Multi level nested case classes with multiple nested occurences and non generic types" - {
      case class A1(x: String, y: A2, z: A3)
      case class A2(x: A3, y: Int)
      case class A3(x: Double, y: A4, z: Boolean)
      case class A4()

      case class B1(x: String, y: B2, z: B3)
      case class B2(x: B3, y: Int)
      case class B3(x: Double, y: B4, z: Boolean)
      case class B4()

      val field1 = "a1"
      val field2 = 1.23
      val field3 = true
      val field4 = 2
      val field5 = 1.3
      val field6 = false

      val a1: A1 = A1(field1, A2(A3(field2, A4(), field3), field4), A3(field5, A4(), field6))
      val result = convert[B1].apply(a1)
      assert(result == B1(field1, B2(B3(field2, B4(), field3), field4), B3(field5, B4(), field6)))
    }

    "Sealed traits with one extending type" - {
      object A {
        sealed trait A
        case class X() extends A
      }

      object B {
        sealed trait B
        case class X() extends B
      }

      val testee: A.A = A.X()

      val result: B.B = convert[B.B](testee)
      assert(result == B.X())
    }

    "Sealed traits with multiple extending types" - {
      object A {
        sealed trait A
        case class X() extends A
        case class Y() extends A
        case class Z() extends A
      }

      object B {
        sealed trait B
        case class X() extends B
        case class Y() extends B
        case class Z() extends B
      }

      val testeeX: A.A = A.X()
      val resultX: B.B = convert[B.B](testeeX)
      assert(resultX == B.X())

      val testeeY: A.A = A.Y()
      val resultY: B.B = convert[B.B](testeeY)
      assert(resultY == B.Y())

      val testeeZ: A.A = A.Z()
      val resultZ: B.B = convert[B.B](testeeZ)
      assert(resultZ == B.Z())
    }

    "Sealed traits with one extending type that contains a product type" - {
      object A {
        case class AInner(a: Int, b: String)
        sealed trait A
        case class X(inner: AInner) extends A
      }

      object B {
        case class BInner(a: Int, b: String)
        sealed trait B
        case class X(inner: BInner) extends B
      }

      val testee: A.A = A.X(A.AInner(42, "essa"))
      val result: B.B = convert[B.B](testee)
      assert(result == B.X(B.BInner(42, "essa")))
    }
    "Two sealed traits where one contains the other, including plain types" - {
      object Origin {
        case class Outer(b: B)
        sealed trait A
        case class AX(inner: B, outer: Outer)    extends A
        case class AY(inner1: B, inner2: String) extends A
        sealed trait B
        case class BX()              extends B
        case class BY(inner: Double) extends B
      }

      object Target {
        case class Outer(b: B)
        sealed trait A
        case class AX(inner: B, outer: Outer)    extends A
        case class AY(inner1: B, inner2: String) extends A
        sealed trait B
        case class BX()              extends B
        case class BY(inner: Double) extends B
      }

      val testee1: Origin.A   = Origin.AX(Origin.BX(), Origin.Outer(Origin.BX()))
      val expected1: Target.A = Target.AX(Target.BX(), Target.Outer(Target.BX()))
      val result1             = convert[Target.A](testee1)
      assert(result1 == expected1)

      val field               = "hello"
      val testee2: Origin.A   = Origin.AY(Origin.BX(), field)
      val expected2: Target.A = Target.AY(Target.BX(), field)
      val result2             = convert[Target.A](testee2)
      assert(result2 == expected2)
    }
    "Fields of type HList should not converted by default" - {
      object Origin {
        case class A(hlist: B :: HNil)
        case class B()
      }

      object Target {
        case class A(hlist: B :: HNil)
        case class B()
      }

      val hlist            = Origin.B() :: HNil
      val testee: Origin.A = Origin.A(hlist)
      val testeeNG         = NestedGeneric[Origin.A].apply(testee)
      val expectedNG       = hlist :: HNil
      assert(testeeNG == expectedNG)
      compileError("convert[Target.A](testee)")
    }
    "Fields of type Coproduct should not converted by default" - {
      object Origin {
        case class A(coproduct: B :+: CNil)
        case class B()
      }

      object Target {
        case class A(coproduct: B :+: CNil)
        case class B()
      }

      val coproduct            = Inl(Origin.B())
      val testee: Origin.A = Origin.A(coproduct)
      val testeeNG         = NestedGeneric[Origin.A].apply(testee)
      val expectedNG       = coproduct :: HNil
      assert(testeeNG == expectedNG)
      compileError("convert[Target.A](testee)")
    }
  }
}
