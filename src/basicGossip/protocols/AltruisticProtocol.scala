package basicGossip.protocols

import basicGossip.messages.Info
import basicGossip.node.Usernode
import peersim.config.FastConfig
import peersim.core.CommonState
import peersim.core.Linkable
import peersim.transport.Transport
import scala.util.Random
import basicGossip.oracle.Oracle
import peersim.core.Node
import hyparview.HyParViewJoinTest
import basicGossip.node.Neighbor
import org.omg.PortableInterceptor.ACTIVE
import basicGossip.node.NodeStatus

class AltruisticProtocol(name: String) extends GeneralProtocol {

  override def shouldLookForNewNeighbor(un: Usernode): Boolean = {
    Oracle.nodeHpvProtocol(un.getID.toInt)._2.neighbors.size <= Oracle.MIN_WIN_TO_SEARCH && un.solvingChallenges.size == 0
  }

  override def computeFanout(gossiper: Usernode, sender: Node): Set[Long] = {
    (gossiper.scoreList.filter(x => x._1 != 0 && x._1 != sender.getID) filter {
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

  override def newNodeSolving(id: Int) = {
    
  }
  
  override def canAcceptNewNeighbor(un: Usernode) =
    Oracle.nodeHpvProtocol(un.getID.toInt)._2.neighbors.size <= HyParViewJoinTest.activeViewSize

  override def addToScoreList(un: Usernode, nid: Long) {
    val neigh = Neighbor(Oracle.baseRank.toInt, NodeStatus.ACTIVE)
    un.scoreList = un.scoreList.updated(nid, neigh)
  }
}