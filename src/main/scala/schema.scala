

import scala.reflect.runtime._
import scala.reflect.runtime.{universe => ru}
import io.circe.Json


case class Breakfast(time: String, food: List[Food])
case class Food(name: String, calories: Int)


object Schema extends App {
  /*
   * Extract a json schema from nested case classes.
   * There is https://github.com/coursera/autoschema but it uses play.json instead of circe, does stuff we don't need
   * and we can try to do it ourselves.
   */
  case class Field(name: String, value: Json)

  val stringType = ru.typeOf[String]
  val intType = ru.typeOf[Int]
  val floatType = ru.typeOf[Float]
  val doubleType = ru.typeOf[Double]
  val booleanType = ru.typeOf[Boolean]
  val listType = ru.typeOf[List[_]]
  val vectorType = ru.typeOf[Vector[_]]


  def getJsonType(t: universe.Type): String = {
    val isOptional = t.typeConstructor == ru.typeOf[Option[_]].typeConstructor

    // strip Option
    // TODO: use this information
    val tpe: universe.Type = if (isOptional) {
      t.asInstanceOf[ru.TypeRefApi].args.head
    } else {
      t
    }
    // TODO: do it more nicely
    val jt =
      if      (tpe =:= stringType) { "string" }
      else if (tpe =:= intType) { "integer" }
      else if (tpe =:= floatType || tpe =:= doubleType) { "number" }
      else if (tpe <:< listType || tpe <:< vectorType) { "array" }
      else if (tpe =:= booleanType) { "boolean" }
      else { "object" }
    jt
  }

  def jsonFromFields(fields: Iterable[Field]): Json = {
    Json.fromFields(fields.map(f => (f.name, f.value)))
  }

  def schema(t: universe.Type): Json = {
    val jsonType = getJsonType(t)
    val fieldType = Some(Field("type", Json.fromString(jsonType)))

    val members: Iterable[universe.Symbol] = t.members.filter(m => m.isTerm && m.asTerm.isVal)
    val fieldExtra: Option[Field] = jsonType match {
      case "object" => {
        Some(Field("properties", jsonFromFields(
          members.map(m => Field(m.name.toString.trim, schema(m.typeSignature)))
        )))
      }
      case "array" => Some(Field("items", schema(t.typeArgs.head)))
      case _ => None
    }

    jsonFromFields(List(fieldType, fieldExtra).flatten)
  }

  val root: universe.Type = ru.typeOf[Breakfast]
  val result: Json = schema(root)
  println(result.spaces2)
}

