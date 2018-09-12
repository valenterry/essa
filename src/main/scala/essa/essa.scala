package essa

import shapeless._
import shapeless.ops.hlist.LeftFolder
import shapeless.tag.@@
import util.transform._

object essa {
  final class ConvertPartial[B] {
    def apply[A, NG](in: A)(
        implicit
        lngenA: LabelledNestedGeneric.Aux[A, NG],
        lngenB: LabelledNestedGeneric.Aux[B, NG]
    ): B = {
      lngenB.from(lngenA.to(in))
    }
  }

  def convert[Target] = new ConvertPartial[Target]

  final class ConvertWithTransformationsPartial[Target, TransformationsType](transformations: TransformationsType) {
    def apply[Origin, OriginNG, TargetNG, TFHead, TFTail <: HList](in: Origin)(transformations: TFHead :: TFTail)(
        implicit
        lngenOrigin: LabelledNestedGeneric.Aux[Origin, OriginNG],
        lngenTarget: LabelledNestedGeneric.Aux[Target, TargetNG],
        folder: LeftFolder.Aux[TFHead :: TFTail, (Origin, OriginNG) @@ (Target, TargetNG), TransformationsType, (Origin, TargetNG) @@ (Target, TargetNG)]
    ): Target = {
      val targetNG = folder(transformations, tag[(Target, TargetNG)]((in, lngenOrigin(in))))._2
      lngenTarget.from(targetNG)
    }
  }

  def convertWithTransformations[Target] = new ConvertWithTransformationsPartial[Target, DefaultTransformations.type](DefaultTransformations)

  def convertWithTransformationsCustom[Target, TransformationsType](transformations: TransformationsType) =
    new ConvertWithTransformationsPartial[Target, TransformationsType](transformations)
}
