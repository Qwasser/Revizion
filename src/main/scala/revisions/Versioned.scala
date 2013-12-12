package revisions 

trait Versioned {
  /**
   * Releases current version
   * 
   */
  def releaseCurrent: Unit = Unit
	
}