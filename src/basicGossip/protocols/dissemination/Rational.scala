package basicGossip.protocols.dissemination

import basicGossip.node.Usernode
import peersim.core.Node
import basicGossip.oracle.Oracle

class Rational extends Disseminator {

  override def computeFanout(gossiper: Usernode, sender: Node): Set[Long] = {
    val neighbors = gossiper.scoreList.filter(x => x._1 != 0) filter {
      case (id, neigh) if neigh.score >= Math.abs(Oracle.FR_THRESHOLD) - 2 => true
      case _ => false
    }
    neighbors.keySet
  }
}

object RationalDisseminator extends Rational