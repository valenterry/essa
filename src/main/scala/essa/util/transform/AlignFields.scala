package essa.util.transform

import shapeless._
import shapeless.tag.@@

object AlignFields

trait AlignFields extends Poly2 {
  implicit def byFieldOrder[Origin, Target, NestedGeneric, TargetNG](
      implicit align: AlignRecursive.Aux[NestedGeneric, TargetNG, TargetNG]
  ): Case.Aux[(Origin, NestedGeneric) @@ (Target, TargetNG), AlignFields.type, (Origin, TargetNG) @@ (Target, TargetNG)] = at {
    (aggr: (Origin, NestedGeneric) @@ (Target, TargetNG), transformation: AlignFields.type) =>
      val targetNG: TargetNG = align(aggr._2)
      val origin: Origin     = aggr._1
      tag[(Target, TargetNG)]((origin, targetNG))
  }
}
