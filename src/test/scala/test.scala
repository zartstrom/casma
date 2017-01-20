import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

import shapeless._
import scala.reflect.runtime.{universe => ru}

import base.Schema.schemaFromUniverseType
import base.Breakfast


object ScalaCheckDemo extends Properties("Demo") {



  //property("myprop") = forAll { l: List[Int] =>
  //  l.reverse.reverse == l
  //}

  type ISB = Int :+: String :+: Boolean :+: CNil

  //case class Funny(a: ISB, b: ISB, c: ISB)
  //def genISB: Gen[ISB] = oneOf(arbitrary[Int], arbitrary[String], arbitrary[Boolean])
  //val funny = Funny("s", true, 2)

  //property("parsable") = forAll { l: List[Int] =>
  //  schemaFromUniverseType(t = ru.typeOf[Breakfast], None).spaces2.length >= 5
  //}

}