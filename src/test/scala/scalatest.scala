/**
  * Created by phil on 10/29/16.
  */
package testing

//import collection.mutable.Stack
import org.scalatest._

import scala.reflect.runtime._
import scala.reflect.runtime.{universe => ru}
import base._
import base.Schema._



class StackSpec extends FlatSpec {

  "Breakfast" should "be parsable" in {
    assert(getBreakfast().spaces2.length >= 5)
  }

  "Test case class" should "have type object" in {

    val d = Dummy("yes")
    assert(schar(d).spaces2.length >= 2)
  }
}
