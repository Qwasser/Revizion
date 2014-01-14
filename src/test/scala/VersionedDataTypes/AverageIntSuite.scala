package VersionedDataTypes

import revisions.Revision
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.concurrent._
import scala.concurrent.duration._

@RunWith(classOf[JUnitRunner])
class AverageIntSuite extends FunSuite{
  
  val testVer: AverageInt = new AverageInt
  
  test("Simple put") {
    val testVal = 10
    testVer.setItem(testVal)
    assert(testVer.getItem == testVal)
  }
  
  test("Got average after join") {
    val testVal1 = 10
    val testVal2 = 20
    testVer.setItem(testVal1)
    def task():Unit = {
      testVer.setItem(testVal2)
      assert(testVer.getItem == testVal2)
      }
    val r1 = Revision.mainRevision.fork(task)
    assert(testVer.getItem == testVal1)
    
    Revision.mainRevision.hardJoin(r1)
    assert(testVer.getItem == (testVal2 + testVal1)/2)
  }
  
  test("Netsed revision") {
    val testVal1 = 100
    val testVal2 = 200
    val testVal3 = 300
    testVer.setItem(testVal1)
    
    def task1():Unit = {
      testVer.setItem(testVal2)
      //println ("task1")
      assert(testVer.getItem == testVal2)
      }
    
     def task2():Unit = {
      assert(testVer.getItem == testVal2)
      //println ("task2")
      testVer.setItem(testVal3)
      assert(testVer.getItem == testVal3)
      }
    
    var r1= Revision.mainRevision.fork(task1)
    
    val fork = r1.fork(task2)
    val r2 = fork._1
    r1 = fork._2
    
    assert(testVer.getItem == testVal1)
    
    //println ("wtf")
    r1 = r1.tailJoin(r2)
    Revision.mainRevision.hardJoin(r1)	
    assert(testVer.getItem == ((testVal3 + testVal2)/2 + testVal1)/2)
  }
  
}