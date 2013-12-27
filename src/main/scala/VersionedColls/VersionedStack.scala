package VersionedColls
import revisions.VersionedObj
import revisions.SpecialMerge
import scala.collection.immutable.Stack

trait StackDAO[T] {
  
  this: VersionedStack[T] =>
  
  def push(item: T) : Unit = {
    val stack = this.getItem
    this.setItem(stack.push(item))
  }
  
  def pop() : T = {
    val stack = this.getItem
    
    stack.pop2 match {
      case (item, newStack) =>
        this.setItem(newStack)
        item
    }
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
    println("joiner $joiner")
    println(joiny)
    println(root)
    var resultStack = new Stack[T]
    val joinyIterator = joiny.reverse.iterator
    val rootIterator = root.reverse.iterator
    
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
    	      resultStack = resultStack.push(joinerItem)
    	    } else {
    	      rootOpt match {
    	        case Some(rootItem) =>
    	          if(rootItem == joinyItem) {
    	            resultStack = resultStack.push(rootItem)
    	          } else {
    	            resultStack = resultStack.push(joinerItem)
    	          }
    	          
    	        case None =>
    	          resultStack = resultStack.push(joinerItem)
    	      }
    	    }
    	    
    	  case None =>
    	    resultStack = resultStack.push(joinerItem)
    	}
    }
   
    resultStack
  }
    
}

abstract class VersionedStack[T] extends VersionedObj[Stack[T]] with  StackDAO[T] {
  this.setItem(new Stack[T])
}