/**
  * Created by phil on 1/26/17.
  */
import io.circe.{Json, JsonObject}
import shapeless._

trait Serializer[T] {
  def serialize[T]: Json
}

trait Datatype[T] extends Serializer[T]

object Datatype extends LabelledProductTypeClassCompanion[Datatype] {
  object typeClass extends LabelledProductTypeClass[Datatype] {
    def emptyProduct: Datatype[HNil] = new Datatype[HNil] {
      def serialize[HNil]: Json = Json.fromJsonObject(JsonObject.empty)
      //def serialize(value: HNil): Json = Json.fromJsonObject(JsonObject.empty)
    }

    def product[H, T <: HList](
        name: String,
        dh: Datatype[H],
        dt: Datatype[T]
    ): Datatype[H :: T] = new Datatype[H :: T] {
      //def serialize(value: H :: T): Json =
      def serialize[T]: Json =
        Json.obj((name, dh.serialize)).deepMerge(dt.serialize)
    }

    def project[F, G](
        instance: => Datatype[G],
        to: F => G,
        from: G => F
    ): Datatype[F] = new Datatype[F] {
      def serialize[F]: Json = instance.serialize
    }
  }
}

object Gabel {
  implicit object DatatypeString extends Datatype[String] {
    def serialize[String] = Json.obj(("type", Json.fromString("string")))
    //def serialize = Json.obj((value, Json.fromString(value)))
  }
  implicit object DatatypeInt extends Datatype[Int] {
    def serialize[Int] = Json.obj(("type", Json.fromString("integer")))
  }
  implicit object DatatypeOption extends Datatype[Option[_]] {
    def serialize[Option] = Json.obj(("type", Json.fromString("integer")))
  }
  case class Foo(bar: String, blah: String)
  val fooDatatype = implicitly[Datatype[Foo]]

  def main(args: Array[String]): Unit = {
    val x = fooDatatype.serialize //(Foo("sd", "fff"))
    println(x)
  }
}
