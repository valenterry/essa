package essa.util

import shapeless.{Generic, unexpected}

trait NotGeneric[A]
object NotGeneric {
  def apply[In](implicit nlgen: NotGeneric[In]): NotGeneric[In] = nlgen

  implicit def notGeneric[A]: NotGeneric[A] = new NotGeneric[A] {}
  implicit def notGenericAmb1[A](implicit gen: Generic[A]): NotGeneric[A] =
    unexpected
  implicit def notGenericAmb2[A](implicit gen: Generic[A]): NotGeneric[A] =
    unexpected
}
