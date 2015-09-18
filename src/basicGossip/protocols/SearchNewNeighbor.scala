package basicGossip.protocols

import peersim.cdsim.CDProtocol
import peersim.core.Node
import basicGossip.node.Usernode
import basicGossip.oracle.Oracle
import peersim.core.Network
import peersim.edsim.EDProtocol
import scala.util.Random
import hyparview.MyHyParView
import basicGossip.node.NodeStatus
import basicGossip.protocols.GeneralProtocol.Heavyweight
import basicGossip.protocols.GeneralProtocol.H

class SearchNewNeighbor(name: String) extends CDProtocol {

  def nextCycle(node: Node, pid: Int) {
    node match {
      case un: Usernode if un.getID != 0 => execute(un)
      case _ =>
    }
  }

  def execute(un: Usernode) {
//    if (Random.nextInt(10) > 7) {
//      val id = Oracle.getNode(Random.nextInt(Network.size))
//      if (!H.blackList.contains(id.getID) && id.getID != un.getID && id.getID != 0 /*&& !Oracle.altruistics.contains(id.getID)*/) {
//        H.validate(id)
//      }
//    }

    un.kickFreeRiders
    if (un.shouldLookForNewNeighbor) {
      MyHyParView.join(un)
    }
  }

}