package basicGossip.protocols

import basicGossip.node.Neighbor
import basicGossip.node.NodeStatus
import basicGossip.node.Usernode
import basicGossip.oracle.Oracle
import peersim.core.Node
import peersim.config.Configuration

class AltruisticProtocol(name: String) extends GeneralProtocol {

  override val protocolName = ProtocolName.ALT
  override val baseWin = Oracle.MIN_WIN_TO_SEARCH
  override val maxWin = Oracle.MAX_WIN

  override def shouldLookForNewNeighbor(un: Usernode): Boolean = {
    un.scoreList.size < Oracle.MIN_WIN_TO_SEARCH
  }

  override def computeFanout(gossiper: Usernode, sender: Node): Set[Long] = {
    (gossiper.scoreList.filter(x => x._1 != 0 && x._1 != sender.getID && x._2.status == NodeStatus.ACTIVE) filter {
      case (id, neighbor) if neighbor.score >= Oracle.baseRank => aboveBaseRank
      case (id, neighbor) if Oracle.baseRank > neighbor.score && neighbor.score >= Oracle.FR_THRESHOLD => belowBaseRank(neighbor.score)
      case (id, neighbor) if neighbor.score < Oracle.FR_THRESHOLD => false
    }).keySet
  }

  override def canAcceptNewNeighbor(un: Usernode) =
    un.scoreList.filter(_._2.status == NodeStatus.ACTIVE).size < maxWin

}