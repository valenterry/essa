package essa

import util.transform.AlignRecursive

object essa {
  final class ConvertPartial[B] {
    def apply[A, NG](in: A)(implicit
                            lngenA: LabelledNestedGeneric.Aux[A, NG],
                            lngenB: LabelledNestedGeneric.Aux[B, NG]): B = {
      lngenB.from(lngenA.to(in))
    }
  }

  def convert[Target] = new ConvertPartial[Target]

  final class ConvertReorderPartial[B] {
    def apply[A, NGA, NGB, NGResult](in: A)(
        implicit
        lngenA: LabelledNestedGeneric.Aux[A, NGA],
        lngenB: LabelledNestedGeneric.Aux[B, NGB],
        align: AlignRecursive.Aux[NGA, NGB, NGResult],
        lngenResult: LabelledNestedGeneric.Aux[B, NGResult]
    ): B = {
      lngenResult.from(align(lngenA.to(in)))
    }
  }

  def convertReorder[Target] = new ConvertReorderPartial[Target]
}
