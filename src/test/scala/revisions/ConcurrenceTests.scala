package revisions

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop
import org.scalacheck.Prop._

@RunWith(classOf[JUnitRunner])
class ConcurrenceTests extends FunSuite{
  def smallDelay = Gen.choose(0, 50)
  
 val determinanceProp = Prop.forAll(smallDelay, smallDelay, smallDelay)((d1, d2, d3) => nestedMerge(d1, d2, d3) == d3)
  
 def nestedMerge(v1: Int, v2: Int, v3: Int) : Int = 
  {
    val testVer = new VersionedItem[Int]
    testVer.setItem(v1)
    
    def task1():Unit = {
      testVer.setItem(v2)
      Thread.sleep(v1)
    }
    
    def task2():Unit = {
      testVer.setItem(v3)
      Thread.sleep(v2)
    }
    
    val r1: Revision = Revision.mainRevision.fork(task1)
    val r2: Revision = r1.fork(task2)
    
    Thread.sleep(v3)
    
    r1.tailJoin(r2)
    
    Revision.mainRevision.hardJoin(r1)	
    testVer.getItem(Revision.mainRevision)
  }
  
  test("Concuurent merge is determenistic") {
     determinanceProp.check
  }
}