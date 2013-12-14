package revisions 

import scala.collection.mutable.HashMap

trait Versioned {
  /**
   * Releases current version
   * 
   */
  def releaseCurrent(branch: Branch): Unit 
  
  def collapse(rev: Revision, branch: Branch): Unit
	
  def merge(joiny :Revision, branch: Branch): Unit
  
  
}

trait VersionedItem[T] extends Versioned {
  
  /**
   * Map of item versions
   */
  private val versions: HashMap[Int, T] = new HashMap()
  
  /**
   * Evaluates current revision in threadlocal context
   */
  def rev: Revision = Revision.currentRevision.value
  /**
   * Sets new value to item and additionally creates it's new version
   */
  def setItem(item: T): Unit = {

    // We want to notify branch, that item was written and new version of it was created
    // Later we will join versions using this knowledge
    if (!versions.contains(rev.getVersion)) 
      rev.current.write(this)
    versions.update(rev.getVersion, item) //creating new version	
  }
  
  /**
   * Gets item in the context of current version
   */
  
  def getItem(): T = {
    val v: Int = this.getLatestVersionId(rev.current) 
    versions.get(v) match {
      case Some(item) => item
      case None => throw new java.lang.NullPointerException 
    }
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
  def merge(joiny :Revision, branch: Branch): Unit = {
    val latestWrite = getLatestVersionId(joiny.current)
    
    if(latestWrite == branch.currentVersion)
      this.setItem(versions.get(branch.currentVersion).get)//do we need to check?
  }
  
  /**
   * Releases branch
   */
  def Release(branch: Branch): Unit = {
    this.versions.remove(branch.currentVersion)
  }
  
  /**
   * Collapses branch
   */
  
  def Collapse(branch: Branch): Unit = {
    if (!this.versions.contains(rev.current.currentVersion))
      setItem(versions.get(branch.currentVersion).get)
    this.versions.remove(branch.currentVersion)
  }
  
}