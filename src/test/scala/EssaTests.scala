import essa.essa.convert
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

      val testee = A()
      val result: B = convert[B](testee)
      success
    }

    "Case classes with one field" - {
      case class A(a: String)
      case class B(a: String)

      val field = "hi"
      val result: B = convert[B](A("hi"))
      assert(result == B(field))
    }

    "Case classes with multiple fields" - {
      case class A(a: String, b: Double)
      case class B(a: String, b: Double)

      val field1 = "hi"
      val field2 = 42.2
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
  }
}
