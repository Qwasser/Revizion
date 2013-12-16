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
  
  def fork(task: ()=> Unit): Revision = {
    
    //make new branches to track versioned object modifications
    this.current = Branch(current)
    val newRev = Revision(current, Branch(current))
    
    val f: Future[Unit] = this.task.future.flatMap(u => future {
      Revision.currentRevision.withValue(newRev){
        task.apply
      }
    })
    
    newRev.task = Promise[Unit]
    newRev.task.completeWith(f)
    newRev
  }
  
  /**
   * allows to continue current task with new One
   */
  def continueWith(task: ()=> Unit): Unit = {
    val f: Future[Unit] = future {
      Revision.currentRevision.withValue(this){
        task.apply
      }
    }
    
    this.task.completeWith(f)
  }
  
  /**
   * one revision falls into another after both complite their task
   */
  def tailJoin(joiny: Revision): Unit = {  
    
     val f =  this.task.future.flatMap(u =>
       joiny.task.future).flatMap(u => 
         future{
            recursiveMerge(joiny, joiny.current)
            //println(joiny.current.currentVersion)
            joiny.current.release
            
            current.collapse(this)
         })
     
     this.task = Promise[Unit]
     this.task.completeWith(f)
  }
  
  def hardJoin(joiny: Revision): Unit = {
    //Await.result(this.task.future, 1.seconds)
    Await.result(joiny.task.future, 2.seconds)
    recursiveMerge(joiny, joiny.current)
    joiny.current.release
    current.collapse(this)
    
  } 
  
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
  
  def apply(root: Branch, current: Branch): Revision = {
    val rev = new Revision(root)
    rev.current = current
    rev
  }
  
  val mainRevision: Revision = {
    new Revision(Branch.apply())
  }
  
  val currentRevision = new DynamicVariable[Revision](mainRevision)
}