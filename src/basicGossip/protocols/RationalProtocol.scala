package basicGossip.protocols

import basicGossip.node.Neighbor
import basicGossip.node.NodeStatus
import basicGossip.node.Usernode
import basicGossip.oracle.Oracle
import basicGossip.protocols.GeneralProtocol.Lightweight
import basicGossip.protocols.GeneralProtocol.ProtocolName
import basicGossip.protocols.dissemination.AltruisticDisseminator
import basicGossip.protocols.dissemination.RationalDisseminator

class RationalProtocol(name: String) extends Lightweight(name) {

  override val baseWin = Oracle.RACIONAL_MAX_CONNECTIONS
  override val maxWin = Oracle.RACIONAL_MAX_CONNECTIONS
  override val protocolName = ProtocolName.FR
  override val disseminator = RationalDisseminator
  
  override def shouldLookForNewNeighbor(un: Usernode): Boolean = {
    un.scoreList.size < baseWin
  }

  override def canAcceptNewNeighbor(un: Usernode) = {
    un.scoreList.filter(_._2.status == NodeStatus.ACTIVE).size < baseWin
  }
  
  override def swapToLifting(un: Usernode) = {
    //rationals do not swap
  }
}