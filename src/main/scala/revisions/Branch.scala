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
  
  def getWritten(): List[Versioned] = {
    val s = written
    s
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
      written.foreach(_.releaseCurrent(this))
    }
    
    this match {
      case ParentedBranch(parent) => parent.release 
    }	  
  }
  
  def collapse(main : Revision): Unit = {
    collapseHelper(main, this)  
  }
  
  private def collapseHelper(rev: Revision, branch: Branch):Unit = {
    if (branch.getParent.currentVersion != rev.root.currentVersion && branch.getParent.refCount == 1){
      branch.getParent.written.foreach(_.collapse(rev, branch.getParent))
      branch.getParent.setParent(branch.getParent.getParent)
      collapseHelper(rev, branch)
    }
  }
  
  def getParent: Branch = {
    this match{
      case ParentedBranch(parent) => parent
      case RootBranch() => throw new java.lang.NullPointerException
    }
  }
  
  def setParent(parent: Branch):Unit = {
    this match{
      case ParentedBranch(p) => ParentedBranch(p).parent = parent
      case RootBranch() => throw new java.lang.IllegalArgumentException
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
case class ParentedBranch(var parent: Branch) extends Branch {
  
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


