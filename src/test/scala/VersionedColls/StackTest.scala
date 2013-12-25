package VersionedColls

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.concurrent._
import scala.concurrent.duration._

@RunWith(classOf[JUnitRunner])
class StackTest extends FunSuite{
	class BasicVStack[T] extends VersionedStack[T] with StackMerge[T] 
	val stack: BasicVStack[Int] = new BasicVStack[Int] 
	
	
  test("Push and pull") {
    val testVal = 10
    stack.push(testVal)
    assert(stack.pop == testVal)
  }
	
}