package VersionedDataTypes

import revisions.VersionedObj
import revisions.SpecialMerge

/**
 * Simple merge, that overrides current revision with child
 */
trait SimpleMerge[T] extends SpecialMerge[T]{
  override def mergeFunction(joiner: T, joiny: T, root: T): T = {
    joiny    
  }
}

/**
 * General versioned datatype
 */
class VersionedItem[T] extends VersionedObj[T] with SimpleMerge[T] 