package basicGossip.protocols

import basicGossip.node.Neighbor
import basicGossip.node.NodeStatus
import basicGossip.node.Usernode
import basicGossip.oracle.Oracle
import peersim.core.Node

class RationalProtocol(name: String) extends GeneralProtocol {

  override val baseWin = Oracle.RACIONAL_MAX_CONNECTIONS
  override val maxWin = Oracle.RACIONAL_MAX_CONNECTIONS
  
  override def initializeScoreList(un: Usernode, ids: Seq[Long]) = {
    un.scoreList = Map(ids map {
      id => id -> Neighbor(Oracle.baseRank.toInt, NodeStatus.ACTIVE)
    }: _*)
  }

  override def addToScoreList(un: Usernode, nid: Long) {
    val neigh = Neighbor(Oracle.baseRank.toInt, NodeStatus.ACTIVE)
    un.scoreList = un.scoreList.updated(nid, neigh)
  }
  
  override def canAcceptNewNeighbor(un: Usernode) = {
    un.scoreList.filter(_._2.status == NodeStatus.ACTIVE).size < baseWin    
  }

  override def computeFanout(gossiper: Usernode, sender: Node): Set[Long] = {
    val neighbors = gossiper.scoreList.filter(x => x._1 != 0) filter {
      case (id, neigh) if neigh.score >= 13 => true
      case _ => false
    }
    neighbors.keySet
  }
  override def shouldLookForNewNeighbor(un: Usernode): Boolean = {
   un.scoreList.size < baseWin
  }
}