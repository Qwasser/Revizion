package revisions 

import java.util.concurrent.atomic.AtomicInteger

/**
 * Class that tracks amount of written versions and creates 
 * versions on write
 */
abstract class Branch {
  
  /**
   * List of written versioned objects (if object was not written
   * we do not need to merge it)
   */
  var written: List[Versioned] = List()
  
  /**
   * amount of branches pointing on this Branch
   */
  val refCount: AtomicInteger = new AtomicInteger(0)		
  
  /**
   * Current version id
   */
  val currentVersion: Int = Branch.versionCount.getAndIncrement()
  
  /**
   * Releases versions if they are not needed
   */
  def release: Unit = {
    if (refCount.decrementAndGet() == 0){
      written.foreach(_.releaseCurrent)
    }
    
    this match {
      case ParentedBranch(parent) => parent.release 
    }
		  
  }
}

/**
 * Root branch, that has no parent
 */
case class RootBranch extends Branch

/**
 * Branch with parent
 */
case class ParentedBranch(parent: Branch) extends Branch {
  
  parent.refCount.incrementAndGet()
}

/**
 * Companion object for branches
 * Holds version count 
 */
object Branch{
  val versionCount: AtomicInteger = new AtomicInteger(0)
}


