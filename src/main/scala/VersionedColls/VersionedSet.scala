package VersionedColls
import revisions.VersionedObj
import revisions.SpecialMerge

/**
 * Proxies set functions to scala set contained in versioned object
 */
trait SetDAO[T] {
  
  this: VersionedSet[T] =>
    
  def add(item: T) {
    val set = this.getItem
    this.setItem(set + item)
  }
  
  def contains(item: T): Boolean = {
    this.getItem.contains(item)
  }
  
  def remove(item: T) = {
    val set = this.getItem
    this.setItem(set - item)
  }
  
  def isEmpty(): Boolean = {
    this.getItem.isEmpty
  }
  
  def size(): Int = {
    this.getItem.size
  }
}

/**
 * Merges sets by union
 */
trait UnionMerge[T] extends SpecialMerge[Set[T]] {
    override def mergeFunction(joiner: Set[T], joiny: Set[T], root: Set[T]): Set[T] = {
      joiner.union(joiny)
    } 
}

abstract class VersionedSet[T] extends VersionedObj[Set[T]] with SetDAO[T]{
	this.setItem(Set[T]())
}

