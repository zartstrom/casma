/**
  * Created by phil on 10/29/16.
  */
package testing

import org.scalatest._

import base.Schema._
import io.circe.{Json, JsonObject}

class StackSpec extends FlatSpec {

  "Test case class" should "have type object" in {
    case class Funky(message: String)
    val d = Funky("yes")
    val schema: Json = schemaFromInstance(d)

    assert(schema.isObject)
    val obj = schema.asObject.get

    assert(obj.contains("properties"))
    assert(obj.contains("title"))
    assert(obj.contains("type"))

    assert(obj.toMap.get("type").get.asString.get == "object")
    assert(obj.toMap.get("title").get.asString.get == "Funky")
  }

  "Creating a schema" should "include optional fields" in {
    case class Fancy(count: Option[Int])
    val fancy = Fancy(None)
    val schema: Json = schemaFromInstance(fancy)

    assert(schema.isObject)
    val obj: JsonObject = schema.asObject.get

    assert(obj.contains("properties"))
    assert(obj.contains("title"))
    assert(obj.contains("type"))

    val properties: JsonObject = obj.toMap.get("properties").get.asObject.get

    assert(properties.toMap.contains("count"))
    val count: JsonObject = properties.toMap.get("count").get.asObject.get
    assert(count.contains("type"))
  }

  "Creating a schema" should "list required fields" in {
    case class Letters(a: Int, b: Int, c: Option[Int], d: Int)
    val letters = Letters(1, 2, Some(3), 4)
    val schema: Json = schemaFromInstance(letters)

    val obj: JsonObject = schema.asObject.get
    assert(obj.contains("required"))
    val required: List[String] = obj.toMap.get("required").get.asArray.get.map(j => j.asString.get).toList
    assert(required == List("a", "b", "d"))
  }
}
