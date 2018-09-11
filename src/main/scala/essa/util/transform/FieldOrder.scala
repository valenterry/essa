package essa.util.transform

import shapeless._
import shapeless.ops.hlist.Align

//object FieldOrder
//
//object transformation extends Poly1 {
//  implicit def byFieldOrder[Origin, NestedGeneric, Target, Result](
//      implicit
//      align: Align.Aux[NestedGeneric, Target]
//  ): Case.Aux[(Origin, NestedGeneric), Result] = at { input: (Origin, NestedGeneric) =>
//    patch(base, rs.kt :: HNil)
//  }
//}
