package essa

import shapeless.labelled.FieldType
import shapeless._
import OTagged.OT

trait ReplaceOTags[NestedGeneric, Target] extends DepFn1[NestedGeneric] with Serializable

trait LowPriorityReplaceOTypes {
  type Aux[NestedGeneric, Target, Out0] =
    ReplaceOTags[NestedGeneric, Target] { type Out = Out0 }

  implicit def notGeneric[OrigAndTarget]: Aux[OT[OrigAndTarget, OrigAndTarget], OrigAndTarget, OT[OrigAndTarget, OrigAndTarget]] = {
    new ReplaceOTags[OT[OrigAndTarget, OrigAndTarget], OrigAndTarget] {
      type Out = OT[OrigAndTarget, OrigAndTarget]
      def apply(in: OT[OrigAndTarget, OrigAndTarget]): Out = in
    }
  }
}

object ReplaceOTags extends LowPriorityReplaceOTypes {
  def apply[NestedGeneric, Target](implicit ev: ReplaceOTags[NestedGeneric, Target]): Aux[NestedGeneric, Target, ev.Out] = ev

  override type Aux[NestedGeneric, Target, Out0] =
    ReplaceOTags[NestedGeneric, Target] { type Out = Out0 }

  implicit def hnil[Target]: Aux[HNil, HNil, HNil] =
    new ReplaceOTags[HNil, HNil] {
      type Out = HNil
      def apply(in: HNil): Out = in
    }

  implicit def hcons[HeadKey <: Symbol,
                     HeadValue,
                     HeadValueNG,
                     Tail <: HList,
                     TargetHeadValue,
                     TargetHeadValueNG,
                     TargetTail <: HList,
                     TargetHeadNG,
                     TargetTailNG <: HList](
      implicit
      replaceHeadOTags: ReplaceOTags.Aux[OT[HeadValue, HeadValueNG], TargetHeadValue, OT[TargetHeadValue, TargetHeadValueNG]],
      replaceTailOTags: ReplaceOTags.Aux[Tail, TargetTail, TargetTailNG]
  ): Aux[FieldType[HeadKey, OT[HeadValue, HeadValueNG]] :: Tail,
         FieldType[HeadKey, TargetHeadValue] :: TargetTail,
         FieldType[HeadKey, OT[TargetHeadValue, TargetHeadValueNG]] :: TargetTailNG] =
    new ReplaceOTags[FieldType[HeadKey, OT[HeadValue, HeadValueNG]] :: Tail, FieldType[HeadKey, TargetHeadValue] :: TargetTail] {
      type Out = FieldType[HeadKey, OT[TargetHeadValue, TargetHeadValueNG]] :: TargetTailNG
      def apply(in: FieldType[HeadKey, OT[HeadValue, HeadValueNG]] :: Tail): Out = in.asInstanceOf[Out]
    }

  implicit def deriveForNonEmptyProduct[OrigNGValue <: HList, OrigType, Target, TargetLGHead, TargetLGTail <: HList, Replaced <: HList](
      implicit lg: Lazy[LabelledGeneric.Aux[Target, TargetLGHead :: TargetLGTail]],
      replacer: Lazy[ReplaceOTags.Aux[OrigNGValue, TargetLGHead :: TargetLGTail, Replaced]]
  ): Aux[OT[OrigType, OrigNGValue], Target, OT[Target, Replaced]] =
    new ReplaceOTags[OT[OrigType, OrigNGValue], Target] {
      type Out = OT[Target, Replaced]
      def apply(in: OT[OrigType, OrigNGValue]): Out = in.asInstanceOf[Out]
    }

  implicit def deriveForEmptyProduct[OrigType, Target](
      implicit lg: Lazy[LabelledGeneric.Aux[Target, HNil]]
  ): Aux[OT[OrigType, HNil], Target, OT[Target, HNil]] =
    new ReplaceOTags[OT[OrigType, HNil], Target] {
      type Out = OT[Target, HNil]
      def apply(in: OT[OrigType, HNil]): Out = in.asInstanceOf[Out]
    }

  implicit def cnil[Orig, Target]: Aux[CNil, CNil, CNil] =
    new ReplaceOTags[CNil, CNil] {
      type Out = CNil
      def apply(in: CNil): Out = unexpected
    }

  implicit def ccons[HeadKey <: Symbol,
                     HeadValue,
                     HeadValueNG,
                     Tail <: Coproduct,
                     TargetHeadValue,
                     TargetHeadValueNG,
                     TargetTail <: Coproduct,
                     TargetHeadNG,
                     TargetTailNG <: Coproduct](
      implicit
      replaceHeadOTags: ReplaceOTags.Aux[OT[HeadValue, HeadValueNG], TargetHeadValue, OT[TargetHeadValue, TargetHeadValueNG]],
      replaceTailOTags: ReplaceOTags.Aux[Tail, TargetTail, TargetTailNG]
  ): Aux[FieldType[HeadKey, OT[HeadValue, HeadValueNG]] :+: Tail,
         FieldType[HeadKey, TargetHeadValue] :+: TargetTail,
         FieldType[HeadKey, OT[TargetHeadValue, TargetHeadValueNG]] :+: TargetTailNG] =
    new ReplaceOTags[FieldType[HeadKey, OT[HeadValue, HeadValueNG]] :+: Tail, FieldType[HeadKey, TargetHeadValue] :+: TargetTail] {
      type Out = FieldType[HeadKey, OT[TargetHeadValue, TargetHeadValueNG]] :+: TargetTailNG
      def apply(in: FieldType[HeadKey, OT[HeadValue, HeadValueNG]] :+: Tail): Out = in.asInstanceOf[Out]
    }

  implicit def deriveForNonEmptyCoproduct[OrigNGValue <: Coproduct, OrigType, Target, TargetLG <: Coproduct, Replaced <: Coproduct](
      implicit
      lg: Lazy[LabelledGeneric.Aux[Target, TargetLG]],
      replacer: Lazy[ReplaceOTags.Aux[OrigNGValue, TargetLG, Replaced]]
  ): Aux[OT[OrigType, OrigNGValue], Target, OT[Target, Replaced]] =
    new ReplaceOTags[OT[OrigType, OrigNGValue], Target] {
      type Out = OT[Target, Replaced]
      def apply(in: OT[OrigType, OrigNGValue]): Out = in.asInstanceOf[Out]
    }
}
