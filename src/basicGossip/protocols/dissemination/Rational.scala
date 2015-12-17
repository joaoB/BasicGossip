package basicGossip.protocols.dissemination

import basicGossip.node.Usernode
import peersim.core.Node
import basicGossip.oracle.Oracle
import basicGossip.node.NodeStatus

class Rational extends Disseminator {

//  override def computeFanout(gossiper: Usernode, sender: Node): Set[Long] = {
//    val neighbors = gossiper.scoreList.filter(x => x._1 != 0) filter {
//      case (id, neigh) if neigh.score >= Math.abs(Oracle.FR_THRESHOLD) - 2 => true
//      case _ => false
//    }
//    neighbors.keySet
//  }
  
  
    override def computeFanout(gossiper: Usernode, sender: Node): Set[Long] = {

    (gossiper.scoreList.filter(x => x._1 != 0 && x._1 != sender.getID && x._2.status == NodeStatus.ACTIVE) filter {
      case (id, neighbor) if neighbor.score >= Oracle.baseRank => aboveBaseRankRational
      case (id, neighbor) if Oracle.baseRank > neighbor.score && neighbor.score >= Oracle.FR_THRESHOLD => belowBaseRankRational(neighbor.score)
      case (id, neighbor) if neighbor.score < Oracle.FR_THRESHOLD => false
    }).keySet
  }
}

object RationalDisseminator extends Rational