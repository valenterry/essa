package essa

import shapeless.labelled.FieldType
import shapeless.ops.hlist.{Mapper, Zip}
import shapeless.{::, DepFn1, HList, HNil, LabelledGeneric, Lazy, Poly1}
import OTagged.OT
import util.NotLabelledGeneric
import util.NotGeneric

trait ReplaceOTags[OrigType, NestedGeneric, Target] extends DepFn1[OT[OrigType, NestedGeneric]] with Serializable

trait LowPriorityReplaceOTypes {
  type Aux[OrigType, NestedGeneric, Target, Out0] =
    ReplaceOTags[OrigType, NestedGeneric, Target] { type Out = Out0 }

  implicit def forNonGeneric[OrigAndTarget: NotLabelledGeneric: NotGeneric]
    : Aux[OrigAndTarget, OrigAndTarget, OrigAndTarget, OT[OrigAndTarget, OrigAndTarget]] = {
    new ReplaceOTags[OrigAndTarget, OrigAndTarget, OrigAndTarget] {
      type Out = OT[OrigAndTarget, OrigAndTarget]
      def apply(in: OT[OrigAndTarget, OrigAndTarget]): Out = in
    }
  }
}

object ReplaceOTags extends LowPriorityReplaceOTypes {
  def apply[OrigType, NestedGeneric, Target](implicit ev: ReplaceOTags[OrigType, NestedGeneric, Target]): Aux[OrigType, NestedGeneric, Target, ev.Out] = ev

  override type Aux[OrigType, NestedGeneric, Target, Out0] =
    ReplaceOTags[OrigType, NestedGeneric, Target] { type Out = Out0 }

  implicit def hnil[Orig, Target]: Aux[Orig, HNil, Target, OT[Target, HNil]] =
    new ReplaceOTags[Orig, HNil, Target] {
      type Out = OT[Target, HNil]
      def apply(in: OT[Orig, HNil]): Out = {
        in.asInstanceOf[OT[Target, HNil]]
      }
    }

  implicit def nonEmptyRecord[Target, TargetLG <: HList, NestedGeneric <: HList, Zipped <: HList, Orig, Result <: HList](
      implicit lgen: LabelledGeneric.Aux[Target, TargetLG],
      zip: Zip.Aux[NestedGeneric :: TargetLG :: HNil, Zipped],
      nestedFolder: Lazy[Mapper.Aux[changeNGAtoNGB.type, Zipped, Result]]): Aux[Orig, NestedGeneric, Target, OT[Target, Result]] =
    new ReplaceOTags[Orig, NestedGeneric, Target] {
      type Out = OT[Target, Result]
      def apply(in: OT[Orig, NestedGeneric]): Out =
        in.asInstanceOf[OT[Target, Result]]
    }
}

//A is the original type, tagged into the NG of A (called ANG)
//B is the Target type, which a shall be replaced with
object changeNGAtoNGB extends Poly1 {
  implicit def default[Aggr <: HList, A, AAndBKey <: Symbol, ANG, BNG, B](implicit replacer: Lazy[ReplaceOTags.Aux[A, ANG, B, OT[B, BNG]]])
    : Case.Aux[(OT[A, FieldType[AAndBKey, ANG]], FieldType[AAndBKey, B]), OT[B, FieldType[AAndBKey, BNG]]] =
    at { next: (OT[A, FieldType[AAndBKey, ANG]], FieldType[AAndBKey, B]) =>
      val converted: OT[B, FieldType[AAndBKey, BNG]] =
        replacer.value
          .apply(next._1.asInstanceOf[OT[A, ANG]])
          .asInstanceOf[OT[B, FieldType[AAndBKey, BNG]]]
      converted
    }
}
