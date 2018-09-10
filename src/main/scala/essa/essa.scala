package essa

import OTagged._

object essa {
  final class ConvertPartial[B] {
    def apply[A, ANG, BNG](in: A)(implicit
                                          ng: NestedGeneric.Aux[A, OT[A, ANG]],
                                          c: ReplaceOTags.Aux[OT[A, ANG], B, OT[B, BNG]],
                                          ng2: NestedGeneric.Aux[B, OT[B, BNG]]): B = {
      ng2.from(c.apply(ng(in)))
    }
  }

  def convert[Target] = new ConvertPartial[Target]
}
