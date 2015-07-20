package basicGossip.protocols

import basicGossip.node.NodeStatus
import basicGossip.node.Usernode
import basicGossip.oracle.Oracle
import basicGossip.node.Neighbor

trait ScoreListManager {

  def initializeScoreList(un: Usernode, ids: Seq[Long]) = {
    un.scoreList = Map(ids map {
      id => id -> Neighbor(Oracle.baseRank.toInt, NodeStatus.ACTIVE)
    }: _*)
  }

  def addToScoreList(un: Usernode, nid: Long) {
    val neigh = Neighbor(Oracle.baseRank.toInt, NodeStatus.ACTIVE)
    un.scoreList = un.scoreList.updated(nid, neigh)
  }

  def newNodeSolving(un: Usernode, id: Int) = {
    val neigh = Neighbor(Oracle.baseRank.toInt, NodeStatus.WAITING)
    un.scoreList = un.scoreList.updated(id, neigh)
  }

}