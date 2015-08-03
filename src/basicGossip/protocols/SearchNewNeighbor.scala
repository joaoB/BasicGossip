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

class SearchNewNeighbor(name: String) extends CDProtocol {

  def nextCycle(node: Node, pid: Int) {
    node match {
      case un: Usernode if un.getID != 0 => execute(un)
      case _ =>
    }
  }

  def execute(un: Usernode) {

    try {
      if (Random.nextInt(10) > 6) {
        un.behaviorProtocol match {
          case prot: Heavyweight =>
            val id = Oracle.getNode(Random.nextInt(Network.size))
            if (!prot.blackList.contains(id.getID))
              prot.validate(id)
          case _ =>
        }
      }
    } catch {
      case e =>
    }

    un.kickFreeRiders
    if (un.shouldLookForNewNeighbor) {
      MyHyParView.join(un)
    }
  }

}