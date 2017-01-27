/**
  * Created by phil on 1/26/17.
  */
import scala.reflect._
import shapeless._
import shapeless.record._
import shapeless.labelled._
import shapeless.ops.record._

object Pennt {

  case class Foo(x: String, y: Int)

  val generic = LabelledGeneric[Foo]

// Get the field names of a case class with Shapeless
  val keys = implicitly[Keys[generic.Repr]].apply

// Now get the class objects as well
  trait FieldTypes[L <: HList] extends DepFn0 { type Out <: HList }

  object FieldTypes {
    type Aux[L <: HList, Out0 <: HList] = FieldTypes[L] { type Out = Out0 }

    def apply[L <: HList](
        implicit fieldTypes: FieldTypes[L]): Aux[L, fieldTypes.Out] = fieldTypes

    implicit def hnilFieldTypes[L <: HNil]: Aux[L, HNil] = new FieldTypes[L] {
      type Out = HNil

      def apply(): Out = HNil
    }

    implicit def hlistFieldTypes[K, V, Rest <: HList](
        implicit fieldTypesRest: FieldTypes[Rest],
        clazz: ClassTag[V]
    ): Aux[FieldType[K, V] :: Rest, String :: fieldTypesRest.Out] =
      new FieldTypes[FieldType[K, V] :: Rest] {
        type Out = String :: fieldTypesRest.Out

        def apply(): Out = clazz.runtimeClass.getName :: fieldTypesRest()
      }
  }

  val fieldTypes = implicitly[FieldTypes[generic.Repr]].apply

  def main(args: Array[String]): Unit = {
    println("Eingepennt!")
    println(fieldTypes)
    println(keys)
    //val x: Keys[lgen.Repr]#Out = keys
  }
}
