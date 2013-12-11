package revisions 

import java.util.concurrent.atomic.AtomicInteger
/**
 * Class that tracks amount of written versions and creates 
 * versions on write
 */

abstract class Branch {
  
}

/**
 * Root branch, that has no parent
 */
case class RootBranch extends Branch

abstract class WorkBranch extends Branch {
  
  /**
   * List of written versioned objects (if object was not written
   * we do not need to merge it)
   */
  var written: List[Versioned] 
  
  /**
   * amount of branches pointing on this Branch
   */
  var refCount: AtomicInteger			
}

/**
 * Companion object for branches
 * Holds version count 
 */
object Branch{
  var versionCount: AtomicInteger = new AtomicInteger(0)
}


