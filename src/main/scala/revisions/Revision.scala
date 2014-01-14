package revisions 

import scala.util.DynamicVariable
import scala.concurrent.Promise
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.util._
import scala.util.control.NonFatal

/**
 * Class, that handles current revision
 * @param root Branch is root branch of current revision
 */
class Revision(val root: Branch) {
  
  /**
   * latest branch of current revision
   */
  var current: Branch = root
  
  /**
   * Returns latest version of revision
   */
  def getVersion: Int = current.currentVersion
  
  /**
   * revision task
   */
  private var task: Promise[Unit] = Promise[Unit]
  
  task.completeWith(future {})
  
  /**
   * creates new revision and runs task in it
   * as future
   */
  
  def nestedFork(task: ()=> Unit): Future[Revision]= {
    
    //make new branches to track versioned object modifications
    val f1 = this.task.future.flatMap(u => future{
	    val newRev = Revision(current, Branch(current))
	    this.current = Branch(current)
	    
	    val f: Future[Unit] = this.task.future.flatMap(u => future {
	      Revision.currentRevision.withValue(newRev){
	        task.apply
	      }
	    })
	    
	    newRev.task = Promise[Unit]
	    newRev.task.completeWith(f)
	    newRev
  })
  
	//  val f2 = this.task.future.flatMap(u => future{
	//    this.task = Promise[Unit]
	//    this.task.completeWith(f1.map(r => Unit))
	//    this
	//  })
  
  f1
  }
  
  def fork(task: ()=> Unit): Future[Revision]= {

    val newRev = Revision(current, Branch(current))
    this.current = Branch(current)

    //make new branches to track versioned object modifications
    val f1 = future {
      val f: Future[Unit] = future {
        Revision.currentRevision.withValue(newRev) {
          task.apply
        }
      }
      newRev.task = Promise[Unit]
      newRev.task.completeWith(f)
      newRev
    }

    //  val f2 = this.task.future.flatMap(u => future{
    //    this.task = Promise[Unit]
    //    this.task.completeWith(f1.map(r => Unit))
    //    this
    //  })

    f1
  }
  
  
  /**
   * allows to continue current task with new One
   */
  def continueWith(task: ()=> Unit): Revision = {
    val newF = this.task.future.flatMap(u => future {
      Revision.currentRevision.withValue(this){
        task.apply
      }
    })
    this.task = Promise[Unit]
    this.task.completeWith(newF)
    this
  }
  
  /**
   * one revision falls into another after both complete their task
   */
  def tailJoin(joiny: Revision): Revision = {
    val f = Future.sequence(List(this.task.future, joiny.task.future))
    this.task = Promise[Unit]
    this.task.completeWith(f.flatMap(u =>
      future {
        recursiveMerge(joiny, joiny.current)
      }))    
    this
  }
  
  /**
   * Makes current revision wait for other one and join with it
   */
  def hardJoin(joiny: Future[Revision]): Unit = {
    //Await.result(this.task.future, 1.seconds)
    val j = Await.result(joiny, 10.seconds)
    
    Await.result(j.task.future, 10.seconds) 
    recursiveMerge(j, j.current)
    //joiny.current.release
    //current.collapse(this)   
  } 
  
  /**
   * Helper function for merge
   */
  private def recursiveMerge(joiny: Revision, branch: Branch): Unit = {
    if (branch.currentVersion != joiny.root.currentVersion) {
      branch.getWritten().foreach(_.merge(this, joiny, branch))
      branch match {
        case ParentedBranch(parent) => recursiveMerge(joiny, parent)
      	case rootBranch => throw new java.lang.NullPointerException
      }
    }
  }

}



object Revision { 
  implicit class RevFuture(f: Future[Revision]){
	  def fork(task: ()=> Unit): (Future[Revision], Future[Revision]) = {
	    val w1 = this.f.flatMap(f => 
	        f.nestedFork(task) 
	    )    
	    val w2 = this.f.flatMap(f => 
	       	f.task.future.flatMap(u => future{
	        f.task = Promise[Unit]
	        f.task.completeWith(w1.map(r => Unit))
	        f
	       	})
	      
	    )
	    (w1, w2)
	  }
	  
	  def continueWith(task: ()=> Unit): Future[Revision] = {
	    f.map(f => f.continueWith(task))
	  }
	  
	 def tailJoin(joiny: Future[Revision]): Future[Revision] = {
	   val rs = Future.sequence(List(f, joiny)) 
	   rs.map(completedRs => completedRs.head.tailJoin(completedRs.reverse.head))
	 }
  }
  
  def apply(root: Branch, current: Branch): Revision = {
    val rev = new Revision(root)
    rev.current = current
    rev
  }
  
  /**
   * Root revision
   */
  val mainRevision: Revision = {
    new Revision(Branch.apply())
  }
  
  /**
   * Variable, that determines revision in current context
   */
  val currentRevision = new DynamicVariable[Revision](mainRevision)
}