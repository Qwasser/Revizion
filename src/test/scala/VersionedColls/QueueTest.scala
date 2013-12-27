package VersionedColls

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.concurrent._
import scala.concurrent.duration._
import revisions.Revision
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class QueueTest extends FunSuite{
	class BasicVQueue[T] extends VersionedQueue[T] with QueueMerge[T] 
	
	test("Enqueue and dequeue") {
		val queue: BasicVQueue[Int] = new BasicVQueue[Int] 
	    val testVal = 10
	    queue.enqueu(testVal)
	    assert(queue.dequeue == testVal)
	    
	    def task(): Unit = {
	      queue.enqueu(testVal)
	    assert(queue.dequeue == testVal)
		}
	    
	    val r1: Revision = Revision.mainRevision.fork(task)
	}
	
	test ("Merging queues") {
	  val queue: BasicVQueue[Int] = new BasicVQueue[Int] 
	  val testVal1 = 1
	  val testVal2 = 2
	  val testVal3 = 3
	  val testVal4 = 4
	  
	  queue.enqueu(testVal1)
	  queue.enqueu(testVal2)
      def task():Unit = {
        queue.enqueu(testVal3)
        
        }
	   val r1: Revision = Revision.mainRevision.fork(task)
	   queue.enqueu(testVal4)
	   Revision.mainRevision.hardJoin(r1)
	   
	   //println(queue.getItem)
      }
	
	
	
	
}