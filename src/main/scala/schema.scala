package base

import scala.reflect.runtime._
import scala.reflect.runtime.{universe => ru}
import io.circe.Json

case class Breakfast(time: String, food: List[Food])
case class Food(name: String, calories: Int)

sealed trait Payload
case class OptinPayload(emailHash: String,
                        optinType: String,
                        status: String,
                        optinTime: String,
                        optinSource: String,
                        campaign: Option[String])
    extends Payload
case class TechnicalInfo(createdByHost: String)
case class Optin(info: TechnicalInfo, payload: OptinPayload)

object Schema extends App {
  /*
   * Extract a json schema from nested case classes.
   * There is https://github.com/coursera/autoschema but it uses play.json instead of circe, does stuff we don't need
   * and we can try to do it ourselves.
   */
  case class Field(name: String, value: Json)

  //def getType[T: ru.TypeTag](obj: T) = ru.typeOf[T]
  //def getUniverseType[T: ru.WeakTypeTag](obj: T) = ru.weakTypeOf[T]
  def getWeakTypeTag[T: ru.WeakTypeTag](obj: T) = ru.weakTypeTag[T]
  def getTypeTag[T: ru.TypeTag](obj: T) = ru.typeTag[T]

  val stringType = ru.typeOf[String]
  val intType = ru.typeOf[Int]
  val floatType = ru.typeOf[Float]
  val doubleType = ru.typeOf[Double]
  val booleanType = ru.typeOf[Boolean]
  val listType = ru.typeOf[List[_]]
  val vectorType = ru.typeOf[Vector[_]]

  val stringSymbol = ru.typeOf[String].typeSymbol
  val intSymbol = ru.typeOf[Int].typeSymbol
  val floatSymbol = ru.typeOf[Float].typeSymbol
  val doubleSymbol = ru.typeOf[Double].typeSymbol
  val booleanSymbol = ru.typeOf[Boolean].typeSymbol
  val listSymbol = ru.typeOf[List[_]].typeSymbol
  val vectorSymbol = ru.typeOf[Vector[_]].typeSymbol

  def getJsonType(t: universe.Type): String = {
    val isOptional = t.typeConstructor == ru.typeOf[Option[_]].typeConstructor
    // strip Option
    // TODO: use this information
    val tpe: universe.Type = if (isOptional) {
      t.asInstanceOf[ru.TypeRefApi].args.head
    } else {
      t
    }
    val sym = tpe.typeSymbol
    // TODO: refactor
    case class JsonTypeMatch(jsonType: String, names: Set[String])
    val poss = List(
      JsonTypeMatch("string", Set("String", "class String")),
      JsonTypeMatch("integer", Set("Int", "class Int")),
      JsonTypeMatch("number",
                    Set("Float", "class Float", "Double", "class Double")),
      JsonTypeMatch("array",
                    Set("Array", "class Array", "Vector", "class Vector")),
      JsonTypeMatch("boolean", Set("Boolean", "class Boolean"))
    )

    // TODO: do it more nicely
    val cand: Set[String] = Set(tpe.toString, sym.toString).filter(_ != null)
    //println(cand)
    //println(poss)
    val res = poss.foldLeft("object")((b: String, j: JsonTypeMatch) => {
      if (cand.intersect(j.names).isEmpty) b else j.jsonType
    })
    res
  }

  def jsonFromFields(fields: Iterable[Field]): Json = {
    Json.fromFields(fields.map(f => (f.name, f.value)))
  }

  def schemaFromUniverseType(t: universe.Type): Json = {
    //println(t)
    val jsonType = getJsonType(t)
    val fieldType = Some(Field("type", Json.fromString(jsonType)))

    val members: Iterable[universe.Symbol] =
      t.members.filter(m => m.isTerm && m.asTerm.isVal)
    val fieldExtra: Option[Field] = jsonType match {
      case "object" => {
        Some(
          Field("properties",
                jsonFromFields(
                  members.map(m =>
                    Field(m.name.toString.trim,
                          schemaFromUniverseType(m.typeSignature)))
                )))
      }
      case "array" =>
        Some(Field("items", schemaFromUniverseType(t.typeArgs.head)))
      case _ => None
    }

    jsonFromFields(List(fieldType, fieldExtra).flatten)
  }

  def getBreakfast(): Json = {
    schemaFromUniverseType(ru.typeOf[Optin])
  }

  //def getWeakType[T: ru.WeakTypeTag](obj: T) = ru.typeOf[T]
  def schemaFromInstance[A](a: A)(implicit ev: ru.WeakTypeTag[A]): Json = {
    //println(ev.tpe)
    schemaFromUniverseType(ev.tpe)
  }

  //val root: universe.Type = ru.weakTypeOf[Optin]
  //val result: Json = schemaFromUniverseType(root)
  //println(result.spaces2)

  val optin = Optin(TechnicalInfo("localhost"),
                    OptinPayload("email", "type", "sl", "alf", "aflj", None))
  val result2: Json = schemaFromInstance(optin)
  println(result2.spaces2)

  def test: Unit = {
    case class Dummy(s: String)
    val d = Dummy("yes")
    val res = schemaFromInstance(d)
    println(res)
  }
  test
}
