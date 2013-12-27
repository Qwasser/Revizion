package VersionedColls

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.concurrent._
import scala.concurrent.duration._
import revisions.Revision

@RunWith(classOf[JUnitRunner])
class StackTest extends FunSuite{
	class BasicVStack[T] extends VersionedStack[T] with StackMerge[T] 
	
	
	
  test("Push and pull") {
	val stack: BasicVStack[Int] = new BasicVStack[Int] 
    val testVal = 10
    stack.push(testVal)
    assert(stack.pop == testVal)
    
    def task(): Unit = {
      stack.push(testVal)
      assert(stack.pop == testVal)
	}
    
    val r1: Revision = Revision.mainRevision.fork(task)
  }
	
	test("Merge test") {
	  val stack: BasicVStack[Int] = new BasicVStack[Int] 
	  stack.push(1)
	  stack.push(2)
	  stack.push(3)
	  
	  def task(): Unit = {
	    stack.pop
	    stack.push(4)
	    stack.push(5)
	  }
	  
	  val r1: Revision = Revision.mainRevision.fork(task)
	  
	  Revision.mainRevision.hardJoin(r1)
	  
	  assert(stack.pop == 5)
	  assert(stack.pop == 4)
	  assert(stack.pop == 2)
	}
	
	/*test("Two Stack Test") {
	  
	}*/
	
}