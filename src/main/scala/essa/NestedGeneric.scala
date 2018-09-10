package essa

import shapeless._

/**
  * Provides a generic representation, recursively for each field of every generic type and otherwise the type itself
  */
trait NestedGeneric[In] extends DepFn1[In] with Serializable {
  def to(in: In): Out = apply(in)
  def from(out: Out): In
}

trait LowPriorityNestedGeneric {
  type Aux[In, Out0] = NestedGeneric[In] { type Out = Out0 }

  implicit def notGeneric[In]: Aux[In, In] = {
    new NestedGeneric[In] {
      type Out = In
      override def apply(in: In): Out = in
      override def from(out: Out): In = out
    }
  }
}

object NestedGeneric extends LowPriorityNestedGeneric {
  def apply[In](implicit ngen: NestedGeneric[In]): Aux[In, ngen.Out] = ngen

  override type Aux[In, Out0] = NestedGeneric[In] { type Out = Out0 }

  implicit def hnil: Aux[HNil, HNil] =
    new NestedGeneric[HNil] {
      type Out = HNil
      override def apply(in: HNil): Out = HNil
      override def from(out: Out): HNil = HNil
    }

  implicit def hcons[Head, Tail <: HList, HeadNG, TailNG <: HList](
      implicit
      ngHead: NestedGeneric.Aux[Head, HeadNG],
      ngTail: NestedGeneric.Aux[Tail, TailNG]
  ): Aux[Head :: Tail, HeadNG :: TailNG] =
    new NestedGeneric[Head :: Tail] {
      type Out = HeadNG :: TailNG
      override def apply(in: Head :: Tail): Out = ngHead(in.head) :: ngTail(in.tail)
      override def from(out: Out): Head :: Tail =
        ngHead.from(out.head) :: ngTail.from(out.tail)
    }

  implicit def cnil: Aux[CNil, CNil] =
    new NestedGeneric[CNil] {
      type Out = CNil
      def apply(in: CNil): Out          = unexpected
      override def from(out: Out): CNil = unexpected
    }

  implicit def ccons[Head, Tail <: Coproduct, HeadNG, TailNG <: Coproduct](
      implicit
      ngHead: NestedGeneric.Aux[Head, HeadNG],
      ngTail: NestedGeneric.Aux[Tail, TailNG]
  ): Aux[Head :+: Tail, HeadNG :+: TailNG] =
    new NestedGeneric[Head :+: Tail] {
      type Out = HeadNG :+: TailNG
      def apply(in: Head :+: Tail): Out =
        in.eliminate(l => Inl(ngHead.apply(l)), r => Inr(ngTail.apply(r)))
      override def from(out: Out): Head :+: Tail =
        out.eliminate(l => Inl(ngHead.from(l)), r => Inr(ngTail.from(r)))
    }

  implicit def deriveForNonEmptyProduct[In, InLGHead, InLGTail <: HList, InNG <: HList](
      implicit
      gen: Lazy[Generic.Aux[In, InLGHead :: InLGTail]],
      ngen: Lazy[NestedGeneric.Aux[InLGHead :: InLGTail, InNG]]
  ): Aux[In, InNG] =
    new NestedGeneric[In] {
      type Out = InNG
      def apply(in: In): Out          = ngen.value.apply(gen.value.to(in))
      override def from(out: Out): In = gen.value.from(ngen.value.from(out))
    }

  implicit def deriveForEmptyProduct[In](implicit gen: Generic.Aux[In, HNil]): Aux[In, HNil] =
    new NestedGeneric[In] {
      type Out = HNil
      def apply(in: In): Out          = HNil
      override def from(out: Out): In = gen.from(out)
    }

  implicit def deriveForNonEmptyCoproduct[In, InLG <: Coproduct, InNG <: Coproduct](
      implicit
      gen: Lazy[Generic.Aux[In, InLG]],
      ngen: Lazy[NestedGeneric.Aux[InLG, InNG]]
  ): Aux[In, InNG] =
    new NestedGeneric[In] {
      type Out = InNG
      def apply(in: In): Out          = ngen.value.apply(gen.value.to(in))
      override def from(out: Out): In = gen.value.from(ngen.value.from(out))
    }
}
