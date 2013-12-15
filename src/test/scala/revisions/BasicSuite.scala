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
}