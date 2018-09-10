package essa

import shapeless.labelled.FieldType
import shapeless._

/**
  * Provides a labelled generic representation, recursively for each field of every labelled generic type and otherwise the type itself
  */
trait LabelledNestedGeneric[In] extends DepFn1[In] with Serializable {
  def to(in: In): Out = apply(in)
  def from(out: Out): In
}

trait LowPriorityNestedGeneric {
  type Aux[In, Out0] = LabelledNestedGeneric[In] { type Out = Out0 }

  implicit def notGeneric[In]: Aux[In, In] = {
    new LabelledNestedGeneric[In] {
      type Out = In
      override def apply(in: In): Out = in
      override def from(out: Out): In = out
    }
  }
}

object LabelledNestedGeneric extends LowPriorityNestedGeneric {
  def apply[In](implicit lngen: LabelledNestedGeneric[In]): Aux[In, lngen.Out] = lngen

  override type Aux[In, Out0] = LabelledNestedGeneric[In] { type Out = Out0 }

  implicit def hnil: Aux[HNil, HNil] =
    new LabelledNestedGeneric[HNil] {
      type Out = HNil
      override def apply(in: HNil): Out = HNil
      override def from(out: Out): HNil = HNil
    }

  implicit def hcons[HeadKey <: Symbol, HeadValue, Tail <: HList, HeadValueNG, TailNG <: HList](
                                                                                                 implicit
                                                                                                 ngHead: LabelledNestedGeneric.Aux[HeadValue, HeadValueNG],
                                                                                                 ngTail: LabelledNestedGeneric.Aux[Tail, TailNG]): Aux[FieldType[HeadKey, HeadValue] :: Tail, FieldType[HeadKey, HeadValueNG] :: TailNG] =
    new LabelledNestedGeneric[FieldType[HeadKey, HeadValue] :: Tail] {
      type Out = FieldType[HeadKey, HeadValueNG] :: TailNG
      override def apply(in: FieldType[HeadKey, HeadValue] :: Tail): Out = ngHead(in.head).asInstanceOf[FieldType[HeadKey, HeadValueNG]] :: ngTail(in.tail)
      override def from(out: Out): FieldType[HeadKey, HeadValue] :: Tail =
        ngHead.from(out.head).asInstanceOf[FieldType[HeadKey, HeadValue]] :: ngTail.from(out.tail)
    }

  implicit def cnil: Aux[CNil, CNil] =
    new LabelledNestedGeneric[CNil] {
      type Out = CNil
      def apply(in: CNil): Out          = unexpected
      override def from(out: Out): CNil = unexpected
    }

  implicit def ccons[HeadKey <: Symbol, HeadValue, Tail <: Coproduct, HeadValueNG, TailNG <: Coproduct](
                                                                                                         implicit
                                                                                                         ngHead: LabelledNestedGeneric.Aux[HeadValue, HeadValueNG],
                                                                                                         ngTail: LabelledNestedGeneric.Aux[Tail, TailNG]): Aux[FieldType[HeadKey, HeadValue] :+: Tail, FieldType[HeadKey, HeadValueNG] :+: TailNG] =
    new LabelledNestedGeneric[FieldType[HeadKey, HeadValue] :+: Tail] {
      type Out = FieldType[HeadKey, HeadValueNG] :+: TailNG
      def apply(in: FieldType[HeadKey, HeadValue] :+: Tail): Out =
        in.eliminate(l => Inl(ngHead.apply(l).asInstanceOf[FieldType[HeadKey, HeadValueNG]]), r => Inr(ngTail.apply(r)))
      override def from(out: Out): FieldType[HeadKey, HeadValue] :+: Tail =
        out.eliminate(l => Inl(ngHead.from(l).asInstanceOf[FieldType[HeadKey, HeadValue]]), r => Inr(ngTail.from(r)))
    }

  implicit def deriveForNonEmptyProduct[In, InLGHead, InLGTail <: HList, InNG <: HList](
      implicit
      lgen: Lazy[LabelledGeneric.Aux[In, InLGHead :: InLGTail]],
      lngen: Lazy[LabelledNestedGeneric.Aux[InLGHead :: InLGTail, InNG]]
  ): Aux[In, InNG] =
    new LabelledNestedGeneric[In] {
      type Out = InNG
      def apply(in: In): Out          = lngen.value.apply(lgen.value.to(in))
      override def from(out: Out): In = lgen.value.from(lngen.value.from(out))
    }

  implicit def deriveForEmptyProduct[In](implicit lgen: LabelledGeneric.Aux[In, HNil]): Aux[In, HNil] =
    new LabelledNestedGeneric[In] {
      type Out = HNil
      def apply(in: In): Out          = HNil
      override def from(out: Out): In = lgen.from(out)
    }

  implicit def deriveForNonEmptyCoproduct[In, InLG <: Coproduct, InNG <: Coproduct](
      implicit
      lgen: Lazy[LabelledGeneric.Aux[In, InLG]],
      lngen: Lazy[LabelledNestedGeneric.Aux[InLG, InNG]]
  ): Aux[In, InNG] =
    new LabelledNestedGeneric[In] {
      type Out = InNG
      def apply(in: In): Out          = lngen.value.apply(lgen.value.to(in))
      override def from(out: Out): In = lgen.value.from(lngen.value.from(out))
    }
}
