package essa

object essa {
  final class ConvertPartial[B] {
    def apply[A, NG](in: A)(implicit
                                          ng: NestedGeneric.Aux[A, NG],
                                          ng2: NestedGeneric.Aux[B, NG]): B = {
      ng2.from(ng.to(in))
    }
  }

  def convert[Target] = new ConvertPartial[Target]
}
