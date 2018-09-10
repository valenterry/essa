package essa

object essa {
  final class ConvertPartial[B] {
    def apply[A, NG](in: A)(implicit
                            lngenA: LabelledNestedGeneric.Aux[A, NG],
                            lngenB: LabelledNestedGeneric.Aux[B, NG]): B = {
      lngenB.from(lngenA.to(in))
    }
  }

  def convert[Target] = new ConvertPartial[Target]
}
