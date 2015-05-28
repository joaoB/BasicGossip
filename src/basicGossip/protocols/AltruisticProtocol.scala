package basicGossip.protocols

import basicGossip.node.Neighbor
import basicGossip.node.NodeStatus
import basicGossip.node.Usernode
import basicGossip.oracle.Oracle
import hyparview.HyParViewJoinTest
import peersim.core.Node

class AltruisticProtocol(name: String) extends GeneralProtocol {

  override def shouldLookForNewNeighbor(un: Usernode): Boolean = {
    Oracle.nodeHpvProtocol(un.getID.toInt)._2.neighbors.size <= Oracle.MIN_WIN_TO_SEARCH
  }

  override def computeFanout(gossiper: Usernode, sender: Node): Set[Long] = {
    (gossiper.scoreList.filter(x => x._1 != 0 && x._1 != sender.getID && x._2.status == NodeStatus.ACTIVE) filter {
      case (id, neighbor) if neighbor.score >= Oracle.baseRank => aboveBaseRank
      case (id, neighbor) if Oracle.baseRank > neighbor.score && neighbor.score >= Oracle.FR_THRESHOLD => belowBaseRank(neighbor.score)
      case (id, neighbor) if neighbor.score < Oracle.FR_THRESHOLD => false
    }).keySet
  }

  override def initializeScoreList(un: Usernode, ids: Seq[Long]) = {
    un.scoreList = Map(ids map {
      id => id -> Neighbor(Oracle.baseRank.toInt, NodeStatus.ACTIVE)
    }: _*)
  }

  override def newNodeSolving(un: Usernode, id: Int) = {
    val neigh = Neighbor(Oracle.baseRank.toInt, NodeStatus.WAITING)
    un.scoreList = un.scoreList.updated(id, neigh)
  }

  override def canAcceptNewNeighbor(un: Usernode) =
    un.scoreList.filter(_._2.status == NodeStatus.ACTIVE).size < 15
    //Oracle.nodeHpvProtocol(un.getID.toInt)._2.neighbors.size <= HyParViewJoinTest.activeViewSize

  override def addToScoreList(un: Usernode, nid: Long) {
    val neigh = Neighbor(Oracle.baseRank.toInt, NodeStatus.ACTIVE)
    un.scoreList = un.scoreList.updated(nid, neigh)
  }
}