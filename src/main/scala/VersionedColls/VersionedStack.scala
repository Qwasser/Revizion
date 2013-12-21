package VersionedColls
import revisions.VersionedObj
import revisions.SpecialMerge
import scala.collection.immutable.Stack
import scala.collection.mutable.Queue

trait StackDAO[T] {
  this: VersionedStack[T] =>
  
  def push(item: T) : Unit = {
    val stack = this.getItem
    
    this.setItem(stack.push(item))
  }
  
  def pop() : T = {
    val tuple = this.getItem.pop2
    
    tuple match {
      case (item, stack) => 
        this.setItem(stack)
        item
    }
  }
}

trait StackMerge[T] extends SpecialMerge[Stack[T]] {
  override def mergeFunction(joiner: Stack[T], joiny: Stack[T], root: Stack[T]) : Stack[T] = {
    val zipSeq = joiner zip joiny
    var resultList = List[T]()
    
    /*for(itemPair <- zipSeq) {
      itemPair match {
        case (joinerItem, joinyItem) =>
          if(joinerItem == joinyItem) {
            resultList ::= joinerItem
          } else {
            resultList ::= joinerItem
            resultList ::= joinyItem
          }
      }
    }*/
    root
    //new Stack[T](resultList.to)
  }
    
}

abstract class VersionedStack[T] extends VersionedObj[Stack[T]] with  StackDAO[T]