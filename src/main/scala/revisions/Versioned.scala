package revisions 

import scala.collection.mutable.HashMap
import scala.collection.concurrent.TrieMap
import scala.concurrent.duration._
import scala.concurrent._
import ExecutionContext.Implicits.global

trait SpecialMerge[T]{
    type self <: VersionedObj[T]
    
	def mergeFunction(joiner: T, joiny: T): T
	
}

trait SimpleMerge[T] extends SpecialMerge[T]{
  override def mergeFunction(joiner: T, joiny: T): T = {
    joiny
  }
}

trait Versioned {
  /**
   * Releases current version
   * 
   */
  def release(branch: Branch): Unit 
  
  def collapse(rev: Revision, branch: Branch): Unit
	
  def merge(rev: Revision, joiny :Revision, branch: Branch): Unit
  
  
}
class VersionedItem[T] extends VersionedObj[T] with SimpleMerge[T] 

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
  def mergeFunction(joiner: T, joiny: T): T
  
  def setItem(item: T): Unit ={
    setItem(item, this.rev)
  }
  
  /**
   * Gets item in the context of current version
   */
  
  def getItem(rev: Revision): T = {
    val v: Int = this.getLatestVersionId(rev.current) 
    versions.get(v) match {
      case Some(item) => item
      case None => throw new java.lang.NullPointerException 
    }
  }
  
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
   * Simple join function
   */
  def merge(rev: Revision, joiny :Revision, branch: Branch): Unit = {
    val latestWrite = getLatestVersionId(joiny.current)
    val mergedItem = mergeFunction(getItem(rev), versions.get(branch.currentVersion).get)
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