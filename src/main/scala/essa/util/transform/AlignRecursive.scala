package essa.util.transform

import shapeless._
import shapeless.labelled.FieldType
import shapeless.ops.record.{AlignByKeys, Keys}

trait AlignRecursive[OriginNG, TargetNG] extends DepFn1[OriginNG] with Serializable

trait LowPriorityRecursiveAlign {
  type Aux[OriginNG, TargetNG, Out0] = AlignRecursive[OriginNG, TargetNG] { type Out = Out0 }

  implicit def notGeneric[NonGeneric]: Aux[NonGeneric, NonGeneric, NonGeneric] = {
    new AlignRecursive[NonGeneric, NonGeneric] {
      type Out = NonGeneric
      override def apply(in: NonGeneric): Out = in
    }
  }
}

object AlignRecursive extends LowPriorityRecursiveAlign {
  def apply[OriginNG, TargetNG](implicit ngen: AlignRecursive[OriginNG, TargetNG]): AlignRecursive[OriginNG, TargetNG] = ngen

  implicit def hnil: Aux[HNil, HNil, HNil] =
    new AlignRecursive[HNil, HNil] {
      type Out = HNil
      override def apply(in: HNil): Out = HNil
    }

  implicit def hcons[TargetHeadKey <: Symbol,
                     OriginHead,
                     OriginTail <: HList,
                     TargetHead,
                     TargetTail <: HList,
                     TargetKeys <: HList,
                     OriginHeadKey <: Symbol,
                     OriginOrderedHeadKey <: Symbol,
                     OriginOrderedHead,
                     OriginOrderedTail <: HList,
                     OriginOrderedHeadAligned,
                     OriginOrderedTailAligned <: HList](
      implicit
      keys: Keys.Aux[FieldType[TargetHeadKey, TargetHead] :: TargetTail, TargetKeys],
      alignByKeys: AlignByKeys.Aux[FieldType[OriginHeadKey, OriginHead] :: OriginTail,
                                   TargetKeys,
                                   FieldType[OriginOrderedHeadKey, OriginOrderedHead] :: OriginOrderedTail],
      alignHead: AlignRecursive.Aux[OriginOrderedHead, TargetHead, OriginOrderedHeadAligned],
      alignTail: AlignRecursive.Aux[OriginOrderedTail, TargetTail, OriginOrderedTailAligned]
  ): Aux[FieldType[OriginHeadKey, OriginHead] :: OriginTail,
         FieldType[TargetHeadKey, TargetHead] :: TargetTail,
         FieldType[OriginOrderedHeadKey, OriginOrderedHeadAligned] :: OriginOrderedTailAligned] =
    new AlignRecursive[FieldType[OriginHeadKey, OriginHead] :: OriginTail, FieldType[TargetHeadKey, TargetHead] :: TargetTail] {
      type Out = FieldType[OriginOrderedHeadKey, OriginOrderedHeadAligned] :: OriginOrderedTailAligned
      override def apply(in: FieldType[OriginHeadKey, OriginHead] :: OriginTail): Out = {
        val aligned = alignByKeys(in)
        alignHead(aligned.head).asInstanceOf[FieldType[OriginOrderedHeadKey, OriginOrderedHeadAligned]] :: alignTail(aligned.tail)
      }
    }

  implicit def cnil: Aux[CNil, CNil, CNil] =
    new AlignRecursive[CNil, CNil] {
      type Out = CNil
      override def apply(in: CNil): Out = unexpected
    }

  implicit def ccons[OriginAndTargetHeadKey <: Symbol,
                     OriginHeadValue,
                     OriginTail <: Coproduct,
                     TargetHeadValue,
                     TargetTail <: Coproduct,
                     TargetHeadValueAligned,
                     TargetTailAligned <: Coproduct](
      implicit
      ngHead: AlignRecursive.Aux[OriginHeadValue, TargetHeadValue, TargetHeadValueAligned],
      ngTail: AlignRecursive.Aux[OriginTail, TargetTail, TargetTailAligned]
  ): Aux[FieldType[OriginAndTargetHeadKey, OriginHeadValue] :+: OriginTail,
         FieldType[OriginAndTargetHeadKey, TargetHeadValue] :+: TargetTail,
         FieldType[OriginAndTargetHeadKey, TargetHeadValueAligned] :+: TargetTailAligned] =
    new AlignRecursive[FieldType[OriginAndTargetHeadKey, OriginHeadValue] :+: OriginTail, FieldType[OriginAndTargetHeadKey, TargetHeadValue] :+: TargetTail] {
      type Out = FieldType[OriginAndTargetHeadKey, TargetHeadValueAligned] :+: TargetTailAligned
      def apply(in: FieldType[OriginAndTargetHeadKey, OriginHeadValue] :+: OriginTail): Out =
        in.eliminate(l => Inl(ngHead(l).asInstanceOf[FieldType[OriginAndTargetHeadKey, TargetHeadValueAligned]]), r => Inr(ngTail(r)))
    }
}
