package revisions

/**
 * Tests root function in Versioned Object
 */

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

@RunWith(classOf[JUnitRunner])
class RootSuite extends FunSuite{
	/**
	 * Returns root of two merging objects
	 */
	trait RootMerge[T] extends SpecialMerge[T]{
	  override def mergeFunction(joiner: T, joiny: T, root: T): T = {
	    root    
	  }
	}

	/**
	 * General versioned datatype
	 */
	class RootItem[T] extends VersionedObj[T] with RootMerge[T] 
	
	
	test("Root of simple fork") {
	    val testVer = new RootItem[Int]
	    val testVal1 = 10
	    val testVal2 = 20
	    val testVal3 = 30
	    testVer.setItem(testVal1)
	    def task():Unit = {
	      testVer.setItem(testVal3)
	      assert(testVer.getItem == testVal3)
	      }
	    
	    val r1 = Revision.mainRevision.fork(task)
	    testVer.setItem(testVal2)
	    assert(testVer.getItem == testVal2)
	    
	    Revision.mainRevision.hardJoin(r1)
	    assert(testVer.getItem == testVal1)
    }
	
	test("Double fork") {
	  /*
	   * Creating tree like this
	   * root
	   * 10
	   * | \
	   * 20 30
	   * | \
	   * 40 50
	   */
	    val testVer = new RootItem[Int]
	    val testVal1 = 10
	    val testVal2 = 20
	    val testVal3 = 30
	    val testVal4 = 40
	    val testVal5 = 50
	    
	    testVer.setItem(testVal1)
	    
	    def task():Unit = {
	      testVer.setItem(testVal3)
	      assert(testVer.getItem == testVal3)
	    }
	    
	    val r1 = Revision.mainRevision.fork(task)
	    testVer.setItem(testVal2)
	    assert(testVer.getItem == testVal2)
	    
	    def task1():Unit = {
	      testVer.setItem(testVal4)
	      assert(testVer.getItem == testVal4)
	    }
	    
	    val r2 = Revision.mainRevision.fork(task1)
	    testVer.setItem(testVal5)
	    assert(testVer.getItem == testVal5)
	    
	   /*
	   * Joining first fork to main
	   * Tree should look like this:
	   * 10
	   * | 
	   * 20 
	   * | \
	   * 10 50
	   */
	    
	   Revision.mainRevision.hardJoin(r1)
	   assert(testVer.getItem == testVal1)  
	   
	   /*
	   * Joining Second fork to main
	   * Tree should look like this:
	   * 20 
	   */
	    
	   Revision.mainRevision.hardJoin(r2)
	   assert(testVer.getItem == testVal2)  
    }
	
	test("Root within nested revisions") {
	  /*
	   * Creating tree like this
	   * root
	   * 10
	   * | \
	   * 20 30
	   *    | \
	   *    50 60
	   */
	    val testVer = new RootItem[Int]
	    val testVal1 = 10
	    val testVal2 = 20
	    val testVal3 = 30
	    val testVal4 = 40
	    val testVal5 = 50
	    val testVal6 = 60
	    
	    testVer.setItem(testVal1)
	    
	    def task():Unit = {
	      testVer.setItem(testVal3)
	      assert(testVer.getItem == testVal3)
	    }
	    
	    var r1 = Revision.mainRevision.fork(task)
	    testVer.setItem(testVal2)
	    assert(testVer.getItem == testVal2)
	    
	    def task1():Unit = {
	      testVer.setItem(testVal6)
	      assert(testVer.getItem == testVal6)
	    }
	    
	    val fork1 = r1.fork(task)
	    var r2 = fork1._1
	    r1 = fork1._2
	    
	    testVer.setItem(testVal4)
	    assert(testVer.getItem == testVal4)
	    
	    def task2():Unit = {
	      testVer.setItem(testVal5)
	      assert(testVer.getItem == testVal5)
	    }
	    
	    r1 = r1.continueWith(task2)
	    
	   /*
	   * Joining 50 and 60
	   * 10
	   * | \
	   * 20 30
	   */
	   r1 = r1.tailJoin(r2)
	   
	   def task3():Unit = {
	     assert(testVer.getItem == testVal3)
	   }
	   r1 = r1.continueWith(task3) 
	   
	   /*
	   * Joining 40 and 30
	   * 10 
	   */
	   Revision.mainRevision.hardJoin(r1)
	   //assert(testVer.getItem == testVal1)
	}

}