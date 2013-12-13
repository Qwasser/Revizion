package revisions 

import scala.util.DynamicVariable
/**
 * Class, that handles current revision
 */
class Revision(root: Branch) {
  
  var current: Branch = root
  
  def getVersion: Int = current.currentVersion
  
  def doAction(f: () => Unit):Unit = {
    Revision.currentRevision.withValue(this){
      f.apply()
    }
  }

}

object Revision {
  
  val mainRevision: Revision = {
    new Revision(Branch.apply())
  }
  
  val currentRevision = new DynamicVariable[Revision](mainRevision)
}