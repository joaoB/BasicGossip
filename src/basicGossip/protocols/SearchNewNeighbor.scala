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

class SearchNewNeighbor(name: String) extends CDProtocol {

  def nextCycle(node: Node, pid: Int) {
    node match {
      case un: Usernode if un.getID != 0 => execute(un)
      case _ =>
    }
  }

  def execute(un: Usernode) {

    kickFreeRiders(un)

    if (un.shouldLookForNewNeighbor) {
      MyHyParView.join(un)
    }
  }

  def kickFreeRiders(un: Usernode) = {
    un.freeRiders map {
      fr =>
        Oracle.kick(fr.toInt)
        un.removeFromScoreList(fr)
        Oracle.getNode(fr.toInt).removeFromScoreList(un.getID)
    }
  }

}