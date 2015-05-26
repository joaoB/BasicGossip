package basicGossip.protocols

import basicGossip.node.Neighbor
import basicGossip.node.NodeStatus
import basicGossip.node.Usernode
import basicGossip.oracle.Oracle
import peersim.core.Node

class RationalProtocol(name: String) extends GeneralProtocol {

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
    un.solvingChallenges.size == 0
    
  }

  override def computeFanout(gossiper: Usernode, sender: Node): Set[Long] = {
    val neighbors = gossiper.scoreList.filter(x => x._1 != 0) filter {
      case (id, neigh) if neigh.score >= 13 => true
      case _ => false
    }
    //println(neighbors.keySet.size)
    neighbors.keySet
  }
  override def shouldLookForNewNeighbor(un: Usernode): Boolean = {
    canAcceptNewNeighbor(un)
  }
}