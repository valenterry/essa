package essa

import shapeless.labelled.FieldType
import shapeless.ops.hlist.Mapper
import OTagged.OT
import shapeless.{DepFn1, HList, HNil, LabelledGeneric, Lazy, Poly1}
import util.NotLabelledGeneric

/**
  * Provides a generic representation, recursively for each field of every generic type and otherwise the type itself
  */
trait NestedGeneric[In] extends DepFn1[In] with Serializable {
  def to(in: In): Out
  def from(out: Out): In
}

trait LowPriorityNestedGeneric {
  type Aux[In, Out0] = NestedGeneric[In] { type Out = Out0 }

  implicit def notGeneric[In](implicit ev: NotLabelledGeneric[In]): Aux[In, OT[In, In]] = {
    new NestedGeneric[In] {
      type Out = OT[In, In]
      def apply(in: In): Out           = in.asInstanceOf[OT[In, In]]
      override def to(in: In)          = apply(in)
      override def from(out: Out): In = out.asInstanceOf[In]
    }
  }
}

object NestedGeneric extends LowPriorityNestedGeneric {
  def apply[In](implicit ngen: NestedGeneric[In]): Aux[In, ngen.Out] = ngen

  override type Aux[In, Out0] = NestedGeneric[In] { type Out = Out0 }

  implicit def hnil[In](implicit ev: LabelledGeneric.Aux[In, HNil]): Aux[In, OT[In, HNil]] =
    new NestedGeneric[In] {
      type Out = OT[In, HNil]
      def apply(in: In): Out          = HNil.asInstanceOf[OT[In, HNil]]
      override def from(out: Out): In = ev.from(out)
      override def to(in: In): Out    = apply(in)
    }

  implicit def hcons[In, InLG <: HList, Result <: HList](implicit lgen: LabelledGeneric.Aux[In, InLG],
                                                         nestMapper: Lazy[Mapper.Aux[nestTransform.type, InLG, Result]],
                                                         unnestMapper: Lazy[Mapper.Aux[unnestTransform.type, Result, InLG]]): Aux[In, OT[In, Result]] =
    new NestedGeneric[In] {
      type Out = OT[In, Result]
      def apply(in: In): Out = {
        val inLG: InLG = lgen.to(in)
        nestMapper.value.apply(inLG).asInstanceOf[OT[In, Result]]
      }
      override def to(in: In): Out    = apply(in)
      override def from(out: Out): In = lgen.from(unnestMapper.value.apply(out))
    }
}

object nestTransform extends Poly1 {
  implicit def default[Aggr <: HList, FieldKey <: Symbol, NextType, NextEntryNG](
      implicit
      nestedGeneric: Lazy[NestedGeneric.Aux[NextType, OT[NextType, NextEntryNG]]])
    : Case.Aux[FieldType[FieldKey, NextType], OT[NextType, FieldType[FieldKey, NextEntryNG]]] =
    at { nextEntry: FieldType[FieldKey, NextType] =>
      val ngResult = nestedGeneric.value(nextEntry: NextType)
      ngResult.asInstanceOf[OT[NextType, FieldType[FieldKey, NextEntryNG]]]
    }
}

object unnestTransform extends Poly1 {
  implicit def default[Orig, Aggr <: HList, NextEntryNG, FieldKey <: Symbol](
      implicit
      nestedGeneric: Lazy[NestedGeneric.Aux[Orig, OT[Orig, NextEntryNG]]]): Case.Aux[OT[Orig, FieldType[FieldKey, NextEntryNG]], FieldType[FieldKey, Orig]] =
    at { nextEntry: OT[Orig, FieldType[FieldKey, NextEntryNG]] =>
      nestedGeneric.value
        .from(nextEntry)
        .asInstanceOf[FieldType[FieldKey, Orig]]
    }
}
