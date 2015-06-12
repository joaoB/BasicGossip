package basicGossip.protocols

import basicGossip.node.Neighbor
import basicGossip.node.NodeStatus
import basicGossip.node.Usernode
import basicGossip.oracle.Oracle
import peersim.core.Node

class RationalProtocol(name: String) extends GeneralProtocol {

  override val baseWin = Oracle.RACIONAL_MAX_CONNECTIONS
  override val maxWin = Oracle.RACIONAL_MAX_CONNECTIONS
  override val protocolName = ProtocolName.FR

  override def computeFanout(gossiper: Usernode, sender: Node): Set[Long] = {
//        val neighbors = gossiper.scoreList.filter(x => x._1 != 0) filter {
//          case (id, neigh) if neigh.score >= 13 => true
//          case _ => false
//        }
//        neighbors.keySet
    (gossiper.scoreList.filter(x => x._1 != 0 && x._1 != sender.getID && x._2.status == NodeStatus.ACTIVE) filter {
      case (id, neighbor) if neighbor.score >= Oracle.baseRank => aboveBaseRank
      case (id, neighbor) if Oracle.baseRank > neighbor.score && neighbor.score >= Oracle.FR_THRESHOLD => belowBaseRank(neighbor.score)
      case (id, neighbor) if neighbor.score < Oracle.FR_THRESHOLD => false
    }).keySet

  }

  override def shouldLookForNewNeighbor(un: Usernode): Boolean = {
    un.scoreList.size < baseWin
  }

  override def canAcceptNewNeighbor(un: Usernode) = {
    un.scoreList.filter(_._2.status == NodeStatus.ACTIVE).size < baseWin
  }
}