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
  private var written: List[Versioned] = List()
  
  /**
   * Lets branch know that versioned object is written
   */
  def write(obj: Versioned): Unit = {
    written = obj :: written
  } 
  
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
  
  def collapse(main: Revision): Unit = {
       
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
  def apply(): Branch = new RootBranch
  def apply(parent: Branch) = new ParentedBranch(parent)
  
  private val versionCount: AtomicInteger = new AtomicInteger(0)
}


