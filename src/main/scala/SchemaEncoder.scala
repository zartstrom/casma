/**
  * Created by phil on 1/25/17.
  */
package base
import io.circe.Json
import shapeless.{::, HList, HNil, Lazy}
import shapeless.Generic
import shapeless.LabelledGeneric
import shapeless.Witness
import shapeless.labelled.FieldType
import shapeless.ops.record.Keys


trait SchemaEncoder[A] extends Serializable { self =>

  /**
    * Convert a value to JSON.
    */
  def apply(a: A): Json
}

object SchemaEncoder {
  final def apply[A](implicit instance: SchemaEncoder[A]): SchemaEncoder[A] =
    instance

  implicit final val encodeInt: SchemaEncoder[Int] = new SchemaEncoder[Int] {
    final def apply(a: Int): Json =
      Json.obj(("type", Json.fromString("integer")))
  }

  implicit final val encodeString: SchemaEncoder[String] =
    new SchemaEncoder[String] {
      final def apply(a: String): Json =
        Json.obj(("type", Json.fromString("string")))
    }

  //implicit final val encodeGeneric[A]: SchemaEncoder[A] = new SchemaEncoder[A] {

  implicit val hnilEncoder: SchemaEncoder[HNil] =
    new SchemaEncoder[HNil] {
      /**
        * Convert a value to JSON.
        */
      override def apply(a: HNil) = Json.Null
    }

  implicit def hlistEncoder[H, T <: HList](implicit hEncoder: Lazy[SchemaEncoder[H]], tEncoder: SchemaEncoder[T]): SchemaEncoder[H :: T] =
    new SchemaEncoder[H :: T] {
      /**
        * Convert a value to JSON.
        */
      override def apply(a: H :: T) = {
        //Json.fromString("does this work?")
        a match {
          case (h :: t) => hEncoder.value(h)
        }
      }

    }

  implicit def genericEncoder[B, H <: HList](implicit gen: Generic.Aux[B, H], env: Lazy[SchemaEncoder[H]]): SchemaEncoder[B] = {
    new SchemaEncoder[B] {
      /**
        * Convert a value to JSON.
        */
      override def apply(b: B) = {
        println("in genericEncoder")
        println(b)
        // println(env)
        // println(env.value)
        //println(type(env))
        // println(gen)
        // val x: R = gen.to(b)
        // val y: Json = env.value(x)
        // println(x)
        // println(y)
        Json.fromString("df")
      }
    }
  }

}

object syntax {
  // implicit class Indexer[T](collecton: Seq[T])(implicit index: Int)
  implicit class SchemaEncoderOps[A](val wrappedEncodeable: A) extends AnyVal {
    def asSchemaJson(implicit encoder: SchemaEncoder[A]): Json =
      encoder(wrappedEncodeable)
  }
}

object SchemaEncoderMain extends App {
  import syntax._

  //def asSchemaJson[A](a: A)(implicit encoder: SchemaEncoder[A]): Json =
 // encoder(a) // apply
//final def asJsonObject(implicit encoder: ObjectEncoder[A]): JsonObject =
//encoder.encodeObject(wrappedEncodeable)

  override def main(args: Array[String]): Unit = {


    val n: Int = 5
    val x = n.asSchemaJson
    println(x)
    println("---")
    // val reprEncoder: SchemaEncoder[String :: Int :: HNil] = implicitly

    case class Kita(name: String, kids: Int)

    val lg = LabelledGeneric[Kita]
    val keys = implicitly[Keys[lg.Repr]].apply
    println(keys)
    //implicitly[SchemaEncoder[Kita]]
    val k = Kita("Rangers", 37)
    //val k = "Bruuh" :: 37 :: HNil
    val kk = k.asSchemaJson

    println(kk)
  }
}
