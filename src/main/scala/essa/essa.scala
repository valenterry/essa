package essa

import OTagged._

object essa {
  final class ConvertPartial[Target] {
    def apply[A, ANG, BNG, Result](in: A)(
        implicit
        ng: NestedGeneric.Aux[A, OT[A, ANG]],
        c: ReplaceOTags.Aux[A, ANG, Target, OT[Target, BNG]],
        ng2: NestedGeneric.Aux[Target, OT[Target, BNG]]): Target = {
      ng2.from(c.apply(ng.to(in)))
    }
  }

  def convert[Target] = new ConvertPartial[Target]
}
