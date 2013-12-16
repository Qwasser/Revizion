package revisions

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class BasicSuite extends FunSuite{
  
  val testVer: VersionedItem[Int] = new VersionedItem[Int]
  
  test("Simple put") {
    val testVal = 10
    testVer.setItem(testVal)
    assert(testVer.getItem == testVal)
  }
  
  test("Joining revision wins") {
    val testVal1 = 10
    val testVal2 = 20
    testVer.setItem(testVal1)
    def task():Unit = {
      testVer.setItem(testVal2)
      assert(testVer.getItem == testVal2)
      }
    val r1: Revision = Revision.mainRevision.fork(task)
    assert(testVer.getItem == testVal1)
    
    Revision.mainRevision.hardJoin(r1)
    assert(testVer.getItem == testVal2)
  }
  
   test("Netsed revision") {
    val testVal1 = 100
    val testVal2 = 200
    val testVal3 = 300
    testVer.setItem(testVal1)
    
    def task1():Unit = {
      testVer.setItem(testVal2)
      assert(testVer.getItem == testVal2)
      }
    
     def task2():Unit = {
      assert(testVer.getItem == testVal2)
      testVer.setItem(testVal3)
      assert(testVer.getItem == testVal3)
      }
    
    val r1: Revision = Revision.mainRevision.fork(task1)
    val r2: Revision = r1.fork(task2)
    
    assert(testVer.getItem == testVal1)
    
    r1.tailJoin(r2)
    Revision.mainRevision.hardJoin(r1)	
    assert(testVer.getItem == testVal3)
  }
}