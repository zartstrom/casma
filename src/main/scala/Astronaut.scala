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
    def typeEncode: Json
  }
  def createEncoder[A](func: A => Json): JsonEncoder[A] =
    new JsonEncoder[A] {
      def encode(value: A): Json =
        func(value)
      def typeEncode: Json = Json.Null
    }

  trait OptionFinder[A] {
    def tellMeTheTruth(value: A): Boolean
  }
  def createOptionFinder[A](func: A => Boolean): OptionFinder[A] =
    new OptionFinder[A] {
      def tellMeTheTruth(value: A) = func(value)
    }
  implicit def anyFinder[A]: OptionFinder[A] = createOptionFinder(_ => false)
  // implicit def hitFinder[A](implicit finder: OptionFinder[A]): OptionFinder[Option[A]] = createOptionFinder(_ => true)
  implicit def hitFinder[A]: OptionFinder[Option[A]] =
    createOptionFinder(_ => true)

// Base type class instances:
  implicit val stringEncoder: JsonEncoder[String] =
    createEncoder(_ => Json.obj(("type", Json.fromString("string"))))
  //createEncoder(str => Json.fromString(str))
  implicit val doubleEncoder: JsonEncoder[Double] =
    createEncoder(_ => Json.obj(("type", Json.fromString("number"))))
  // createEncoder(num => Json.fromDoubleOrNull(num))
  implicit val intEncoder: JsonEncoder[Int] =
    createEncoder(_ => Json.obj(("type", Json.fromString("integer"))))
  //createEncoder(num => Json.fromInt(num))
  implicit val booleanEncoder: JsonEncoder[Boolean] =
    createEncoder(_ => Json.obj(("type", Json.fromString("boolean"))))
  // createEncoder(bool => Json.fromBoolean(bool))
  implicit def listEncoder[A](
      implicit encoder: JsonEncoder[A]): JsonEncoder[List[A]] =
    createEncoder(
      list =>
        Json.obj(("type", Json.fromString("array")),
                 ("items", encoder.encode(list.head))))  // TODO: need to fix the need to access head

  implicit def optionEncoder[A](
      implicit encoder: JsonEncoder[A]): JsonEncoder[Option[A]] =
    createEncoder(opt => opt.map(encoder.encode).getOrElse(Json.Null))

  trait JsonObjectEncoder[A] extends JsonEncoder[A] {
    def encode(value: A): Json
    def typeEncode = Json.Null
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
      tEncoder: JsonObjectEncoder[T],
      optionFinder: OptionFinder[H]
  ): JsonObjectEncoder[FieldType[K, H] :: T] = {
    val fieldName: String = witness.value.name
    createObjectEncoder { hlist =>
      val head = hEncoder.value.encode(hlist.head)
      val tail: Json = tEncoder.encode(hlist.tail)

      // println(fieldName, optionFinder.tellMeTheTruth(hlist.head))
      val isRequired = !optionFinder.tellMeTheTruth(hlist.head)

      val requiredBefore: List[Json] = (for {
        obj <- tail.asObject
        req <- obj.toMap.get("required")
        arr <- req.asArray
      } yield { arr }).getOrElse(Nil)

      val requiredItems: List[Json] = if (isRequired) {
        Json.fromString(fieldName) :: requiredBefore
      } else requiredBefore
      val required: Json = Json.fromValues(requiredItems)

      val headObject: Json =
        Json.obj(
          ("type", Json.fromString("object")),
          ("required", required),
          ("properties", Json.obj((fieldName, head)))
        )
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
    def asJsonSchema(implicit encoder: JsonEncoder[A]): Json =
      encoder.encode(wrappedEncodeable)
  }
}

object AstronautMain {
  def main(args: Array[String]): Unit = {
    println("Welcome!")
    import syntax._

    case class Bottle(color: String,
                      ml: Option[Int],
                      breakable: List[Option[String]],
                      buuh: Option[List[Int]])
    val b =
      Bottle("blue", Some(1000), List(Some("Yes"), None), Some(List(2, 3, 4)))
    println(b.asJsonSchema)
  }
}
