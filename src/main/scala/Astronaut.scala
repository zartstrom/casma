/**
  * Created by phil on 1/25/17.
  */
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness}
import shapeless.labelled.FieldType
import io.circe.{Json, JsonObject}

object Astronaut {

  /*
// JSON data type:
  sealed trait JsonValue
  final case class JsonObject(fields: List[(String, JsonValue)])
      extends JsonValue
  final case class JsonArray(items: List[JsonValue]) extends JsonValue
  final case class JsonString(value: String) extends JsonValue
  final case class JsonNumber(value: Double) extends JsonValue
  final case class JsonBoolean(value: Boolean) extends JsonValue
  case object JsonNull extends JsonValue
  */
// JSON encoder type class:
  trait JsonEncoder[A] {
    def encode(value: A): Json
  }
  def createEncoder[A](func: A => Json): JsonEncoder[A] =
    new JsonEncoder[A] {
      def encode(value: A): Json =
        func(value)
    }
// Base type class instances:
  implicit val stringEncoder: JsonEncoder[String] =
    createEncoder(str => Json.fromString(str))
  implicit val doubleEncoder: JsonEncoder[Double] =
    createEncoder(num => Json.fromDoubleOrNull(num))
  implicit val intEncoder: JsonEncoder[Int] =
    createEncoder(num => Json.obj(("type", Json.fromString("integer"))))
    //createEncoder(num => Json.fromInt(num))
  implicit val booleanEncoder: JsonEncoder[Boolean] =
    createEncoder(bool => Json.fromBoolean(bool))
  implicit def listEncoder[A](
      implicit encoder: JsonEncoder[A]): JsonEncoder[List[A]] =
    createEncoder(list => Json.fromValues(list.map(encoder.encode)))
  implicit def optionEncoder[A](
      implicit encoder: JsonEncoder[A]): JsonEncoder[Option[A]] =
    createEncoder(opt => opt.map(encoder.encode).getOrElse(Json.Null))

  trait JsonObjectEncoder[A] extends JsonEncoder[A] {
    def encode(value: A): Json
  }
  def createObjectEncoder[A](func: A => Json): JsonObjectEncoder[A] =
    new JsonObjectEncoder[A] {
      def encode(value: A): Json =
        func(value)
    }

  implicit val hnilEncoder: JsonObjectEncoder[HNil] =
    createObjectEncoder(hnil => Json.fromJsonObject(JsonObject.empty))

  implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](
      implicit witness: Witness.Aux[K],
      hEncoder: Lazy[JsonEncoder[H]],
      tEncoder: JsonObjectEncoder[T]
  ): JsonObjectEncoder[FieldType[K, H] :: T] = {
    val fieldName: String = witness.value.name
    createObjectEncoder { hlist =>
      val head = hEncoder.value.encode(hlist.head)
      val tail: Json = tEncoder.encode(hlist.tail)

      val headObject: Json = Json.obj((fieldName, head))
      tail.deepMerge(headObject)
    }
  }
  implicit def genericObjectEncoder[A, H <: HList](
      implicit generic: LabelledGeneric.Aux[A, H],
      hEncoder: Lazy[JsonObjectEncoder[H]]
  ): JsonEncoder[A] =
    createObjectEncoder(value => hEncoder.value.encode(generic.to(value)))
}

object syntax {
  import Astronaut._
  implicit class EncoderOps[A](val wrappedEncodeable: A) extends AnyVal {
    def asJson(implicit encoder: JsonEncoder[A]): Json =
      encoder.encode(wrappedEncodeable)
  }
}

object AstronautMain {
  def main(args: Array[String]): Unit = {
    println("Welcome!")
    import Astronaut._
    import syntax._

    case class Bottle(color: String, ml: Int)
    val b = Bottle("blue", 1000)
    val x = "s".asJson
    println(x)
    println(b.asJson)
  }
}
