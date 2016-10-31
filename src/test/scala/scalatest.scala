/**
  * Created by phil on 10/29/16.
  */
package testing

import collection.mutable.Stack
import org.scalatest._

import scala.reflect.runtime._
import scala.reflect.runtime.{universe => ru}
import base._
import base.Schema._


class StackSpec extends FlatSpec {

  "A Stack" should "pop values in last-in-first-out order" in {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    assert(stack.pop() === 2)
    assert(stack.pop() === 1)
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[String]
    assertThrows[NoSuchElementException] {
      emptyStack.pop()
    }
  }

  "Breakfast" should "be parsable" in {
    assert(getBreakfast().spaces2.length >= 5)
  }
}