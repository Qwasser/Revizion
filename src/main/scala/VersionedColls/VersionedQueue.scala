package VersionedColls
import revisions.VersionedObj
import revisions.SpecialMerge
import scala.collection.immutable.Queue

trait QueueDao[T] {
  this: VersionedQueue[T] =>
    def enqueu(item: T): Unit = {
      val queue = this.getItem
      this.setItem(queue.enqueue(item))
    }
    
    def dequeue(): T = {
      val queue = this.getItem
      
      queue.dequeue match {
        case (item, newQueue) =>
          this.setItem(newQueue)
          item
      }
    }
}

trait QueueMerge[T] extends SpecialMerge[Queue[T]] {
  override def mergeFunction(joiner: Queue[T], joiny: Queue[T], root: Queue[T]) : Queue[T] = {
    println(root)
    println(joiner)
    println(joiny)
    def getFirstRootIndex(queue: Queue[T], root: Queue[T]): Int = {
      var rootIterator = root.iterator
      var index = -1
      
      for(item <- queue) {
	      if(rootIterator.hasNext) {
	        if(item == rootIterator.next) {
	          if(index == -1) {
	            index = joiner.indexOf(item)
	          }        	
	        } else {
	          index = -1
	          rootIterator = root.iterator
	        }
	      } else {
	        index = -1
	        rootIterator = root.iterator
	      }         
	    }
      
      index
    }
    
    val joinerIndex = getFirstRootIndex(joiner, root)
    
    val joinyIndex =  getFirstRootIndex(joiny, root)
      
    var resultQueue = Queue[T]()
    
    if(joinerIndex != -1) {
      resultQueue = joiner.slice(joiner.length - joinerIndex - 1, joiner.length - 1)
      for(joinerItem <- joiner.slice(0, joinerIndex)) {
        resultQueue = resultQueue.enqueue(joinerItem)        
      }
      
      if(joinyIndex != -1) {
        for(joinyItem <- joiny.slice(0, joinyIndex)) {
          resultQueue = resultQueue.enqueue(joinyItem)        
        } 
      } else {
        for(joinyItem <- joiny) {
          resultQueue = resultQueue.enqueue(joinyItem)
        }
      }
      
    } else {
      if(joinyIndex != -1) {
        resultQueue = joiny.slice(joiny.length - joinyIndex - 1, joiny.length - 1)
        for(joinerItem <- joiner) {
          resultQueue = resultQueue.enqueue(joinerItem)
        }        
      } else {
        resultQueue = joiner
        for(joinyItem <- joiny) {
          resultQueue = resultQueue.enqueue(joinyItem)
        }
      }
    }
    
    resultQueue
  }
}

abstract class VersionedQueue[T] extends VersionedObj[Queue[T]] with QueueDao[T]{
	this.setItem(Queue[T]())
}