package VersionedColls

import revisions.Revision

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.concurrent._
import scala.concurrent.duration._

@RunWith(classOf[JUnitRunner])
class SetTest extends FunSuite{
	class UnionVSet[T] extends VersionedSet[T] with UnionMerge[T] 
	val testSet: UnionVSet[Int] = new UnionVSet[Int] 
	
	
  test("Basic set usage") {
    val testVal = 10
    val testVal2 = 20
    
    testSet.add(testVal)
    assert(testSet.contains(testVal))
    assert(!testSet.contains(testVal2))
    
    testSet.remove(testVal)
    
    assert(testSet.getItem == Set())
  }
	
  test("Union of two revisions") {
    val testVal1 = 10
    val testVal2 = 20
    val testVal3 = 40
    val testVal4 = 30
    
    val Set1 = Set(testVal1, testVal2, testVal3, testVal4)

    def task():Unit = {
      testSet.add(testVal3)
      testSet.add(testVal4)
      assert(testSet.getItem == Set(testVal3, testVal4))
      }
    
    val r1: Revision = Revision.mainRevision.fork(task)
    
    testSet.add(testVal1)
    testSet.add(testVal2)
    assert(testSet.getItem == Set(testVal1, testVal2))
    
    Revision.mainRevision.hardJoin(r1)
    assert(testSet.getItem == Set1)
  }
	
}
