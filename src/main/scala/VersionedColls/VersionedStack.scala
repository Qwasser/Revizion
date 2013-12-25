package VersionedColls
import revisions.VersionedObj
import revisions.SpecialMerge
import scala.collection.mutable.Stack
import scala.collection.mutable.Queue
import scala.xml.Null

trait StackDAO[T] {
  
  this: VersionedStack[T] =>
  
  def push(item: T) : Unit = {
    val stack = this.getItem
    
    stack.push(item)
  }
  
  def pop() : T = {
    val stack = this.getItem
    
    stack.pop   
  }
}

trait StackMerge[T] extends SpecialMerge[Stack[T]] {
  
   /**
   * Merge two stacks with rules:
   * if joiner stack item equals joiny stack item, add this item to result stack
   * if joiner stack item not equals joiny stack item, but root stack item equals one of them, add root item to result stack
   * in other cases just add joiner stack item to result stack 
   */
  override def mergeFunction(joiner: Stack[T], joiny: Stack[T], root: Stack[T]) : Stack[T] = {
    val resultStack = new Stack[T]
    val joinyIterator = joiny.reverse.iterator
    val rootIterator = root.reverse.iterator
    val conflictQueue = new Queue[T]
    
    def nextOption(iterator: Iterator[T]): Option[T]  = {
      if(iterator.hasNext) {
        Option(iterator.next)
      } else {
        None
      }
    }
    
  
    for(joinerItem <- joiner.reverse) {
    	val joinyOpt = nextOption(joinyIterator)
    	val rootOpt = nextOption(rootIterator)
    	
    	joinyOpt match {
    	  case Some(joinyItem) =>
    	    if(joinerItem == joinyItem) {
    	      resultStack.push(joinerItem)
    	    } else {
    	      rootOpt match {
    	        case Some(rootItem) =>
    	          if(rootItem == joinyItem) {
    	            resultStack.push(rootItem)
    	          } else {
    	            resultStack.push(joinerItem)
    	          }
    	          
    	        case None =>
    	          resultStack.push(joinerItem)
    	      }
    	    }
    	    
    	  case None =>
    	    resultStack.push(joinerItem)
    	}
    }
   
    resultStack
  }
    
}

abstract class VersionedStack[T] extends VersionedObj[Stack[T]] with  StackDAO[T] {
  this.setItem(new Stack[T])
}