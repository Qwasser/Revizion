package revisions 

import scala.util.DynamicVariable
import scala.concurrent.Promise
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util._
import scala.util.control.NonFatal

/**
 * Class, that handles current revision
 * @param root Branch is root branch of current revision
 */
class Revision(root: Branch) {
  
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
  val task: Promise[Unit] = Promise[Unit]
  
  /**
   * creates new revision and runs task in it
   * as future
   */
  
  def fork(task: ()=> Unit): Revision = {
    
    //make new branches to track versioned object modifications
    this.current = Branch(root)
    val newRev = Revision(root, Branch(root))
    
    val f: Future[Unit] = future {
      Revision.currentRevision.withValue(newRev){
        task.apply
      }
    }
    
    newRev.task.completeWith(f)
    newRev
  }
  
  def join(joiny: Revision): Unit = {  
    this.task.future onComplete {
      case Failure(e) => throw e
      case Success(a) => this.task.completeWith(joiny.task.future)
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