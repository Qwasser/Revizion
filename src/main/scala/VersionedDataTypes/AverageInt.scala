package VersionedDataTypes

import revisions.SpecialMerge
import revisions.VersionedObj

class AverageInt extends VersionedObj[Int] with AverageMerge

trait AverageMerge extends SpecialMerge[Int] {
    override def mergeFunction(joiner: Int, joiny: Int, root: Int): Int = {
      //println ("Lets count")
      (joiner + joiny)/2
    }
}