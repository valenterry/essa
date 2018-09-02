package essa.util

import shapeless.{LabelledGeneric, unexpected}

trait NotLabelledGeneric[A]
object NotLabelledGeneric {
  def apply[In](
      implicit nlgen: NotLabelledGeneric[In]): NotLabelledGeneric[In] = nlgen

  implicit def notLabelledGeneric[A]: NotLabelledGeneric[A] =
    new NotLabelledGeneric[A] {}
  implicit def notLabelledGenericAmb1[A](
      implicit lgen: LabelledGeneric[A]): NotLabelledGeneric[A] = unexpected
  implicit def notLabelledGenericAmb2[A](
      implicit lgen: LabelledGeneric[A]): NotLabelledGeneric[A] = unexpected
}
