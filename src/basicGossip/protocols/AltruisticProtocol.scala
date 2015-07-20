package basicGossip.protocols

import basicGossip.node.NodeStatus
import basicGossip.node.Usernode
import basicGossip.oracle.Oracle
import basicGossip.protocols.GeneralProtocol.Lightweight
import basicGossip.protocols.GeneralProtocol.ProtocolName
import basicGossip.protocols.Lifting.Lifting
import basicGossip.protocols.dissemination.AltruisticDisseminator
import basicGossip.protocols.dissemination.Disseminator

class AltruisticProtocol(name: String) extends Lightweight(name) {

  override val protocolName = ProtocolName.ALT
  override val baseWin = Oracle.MIN_WIN_TO_SEARCH
  override val maxWin = Oracle.MAX_WIN
  override val disseminator: Disseminator = AltruisticDisseminator

  override def shouldLookForNewNeighbor(un: Usernode): Boolean = {
    un.scoreList.size < Oracle.MIN_WIN_TO_SEARCH
  }

  override def canAcceptNewNeighbor(un: Usernode) =
    un.scoreList.filter(_._2.status == NodeStatus.ACTIVE).size < maxWin

  override def swapToLifting(un: Usernode) {
    un.setProtocol(0, new Lifting("Lifting"))
  }

}