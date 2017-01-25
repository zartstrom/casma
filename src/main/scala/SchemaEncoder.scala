/**
  * Created by phil on 1/25/17.
  */
package base
import io.circe.Json

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

  def asSchemaJson[A](a: A)(implicit encoder: SchemaEncoder[A]): Json =
    encoder(a) // apply
//final def asJsonObject(implicit encoder: ObjectEncoder[A]): JsonObject =
//encoder.encodeObject(wrappedEncodeable)

  override def main(args: Array[String]): Unit = {

    val n: Int = 5
    val s = asSchemaJson(n)
    val t = asSchemaJson("w")

    val x = n.asSchemaJson
    println(x)
  }
}
