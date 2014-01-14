package revisions 

import scala.collection.mutable.HashMap
import scala.collection.concurrent.TrieMap
import scala.concurrent.duration._
import scala.concurrent._
import ExecutionContext.Implicits.global


/**
 * Trait, that defines merge function
 * Must be inherited and mixed in versioned object to define special merge function/
 */
trait SpecialMerge[T]{
    type self <: VersionedObj[T]  
	def mergeFunction(joiner: T, joiny: T, root: T): T	
}


/**
 * Interface between revisions and versioned objects
 */
trait Versioned {
  
  /**
   * Releases current version. Used for garbage collection
   */
  def release(branch: Branch): Unit 
 
  /**
   * Collapses linear segment of branches into single one
   */
  def collapse(rev: Revision, branch: Branch): Unit
  
 /**
  * Merges two revisions
  */
  def merge(rev: Revision, joiny :Revision, branch: Branch): Unit
  
  
}

/**
 * Template for versioned object. Must be inherited by user classes. 
 */
abstract class VersionedObj[T] extends Versioned {
  
  /**
   * Map of item versions
   */
  val versions: TrieMap[Int, T] = new TrieMap()
  
  /**
   * Evaluates current revision in threadlocal context
   */
  def rev: Revision = Revision.currentRevision.value
 
  /**
   * Sets new value to item and additionally creates it's new version
   */
  def setItem(item: T, rev: Revision): Unit = {

    // We want to notify branch, that item was written and new version of it was created
    // Later we will join versions using this knowledge
    if (!versions.contains(rev.getVersion)) 
      rev.current.write(this)
    versions.update(rev.getVersion, item) //creating new version	
  }
  
  /**
   * Merge function
   */
  def mergeFunction(joiner: T, joiny: T, root: T): T
  
  /**
   * Sets item to current revision
   */
  def setItem(item: T): Unit ={
    setItem(item, this.rev)
  }
  
 
   /**
   * Gets item in the context of specified version
   */
  def getItem(rev: Revision): T = {
    val v: Int = this.getLatestVersionId(rev.current) 
    versions.get(v) match {
      case Some(item) => item
      case None => throw new java.lang.NullPointerException 
    }
  }
  
 /**
  * Gets item in the context of current version
  */
  def getItem(): T = {
    this.getItem(this.rev)
  }
  
  
  /**
   * Gets latest written version in current branch
   */
  private def getLatestVersionId(startBranch: Branch): Int = {
    if (versions.contains(startBranch.currentVersion))
      startBranch.currentVersion
    else
      startBranch match {
      	case ParentedBranch(parent) => getLatestVersionId(parent)
      	case rootBranch => throw new java.lang.NullPointerException
      }      
  } 
  
  /**
   * Finds root element of two branches
   */
  def rootElem(first: Branch, second: Branch): T = {
    if (first.currentVersion == second.currentVersion) {
      this.versions.get(this.getLatestVersionId(first)).get
    } else {
      val bSet = rootSet(Set(), first)
      val commonBranch = findCommon(bSet, second)
      this.versions.get(this.getLatestVersionId(commonBranch)).get
    }
    
  }
  
  /**
   * Gets all parents in Set
   */
  private def rootSet(bSet: Set[Int], curr: Branch): Set[Int] = {
    if (curr.hasParent) {
      rootSet(bSet + curr.currentVersion, curr.getParent)
    } else {
      bSet + curr.currentVersion
    }
  }
  
  /**
   * Finds common parent, if it in set
   */
  private def findCommon(bSet: Set[Int], curr: Branch): Branch =
  {
    if (bSet.contains(curr.currentVersion)) {
      curr
    } else {
      if (curr.hasParent) {
        findCommon(bSet, curr.getParent)
      }
      else {
        throw new java.lang.IllegalArgumentException
      }
    }
  }
  
  /**
   * Simple join function
   */
  def merge(rev: Revision, joiny :Revision, branch: Branch): Unit = {
    val latestWrite = getLatestVersionId(joiny.current)
    val rootItem = rootElem(rev.current, joiny.current)
    val mergedItem = mergeFunction(getItem(rev), versions.get(branch.currentVersion).get, rootItem) //todo: root
    if(latestWrite == branch.currentVersion)
      this.setItem(mergedItem, rev)//do we need to check?
  }
  
  /**
   * Releases branch
   */
  def release(branch: Branch): Unit = {
    this.versions.remove(branch.currentVersion)
  }
  
  /**
   * Collapses branch
   */
  
  def collapse(rev: Revision, branch: Branch): Unit = {
    if (!this.versions.contains(rev.current.currentVersion))
      setItem(versions.get(branch.currentVersion).get, rev)
    this.versions.remove(branch.currentVersion)
  }
  
}