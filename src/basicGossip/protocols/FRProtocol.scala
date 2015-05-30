package basicGossip.protocols

import basicGossip.messages.Info
import basicGossip.node.Usernode
import basicGossip.oracle.Oracle
import peersim.core.Node

class FRProtocol(name: String) extends AltruisticProtocol(name) with GeneralProtocol {

  override def computeFanout(gossiper: Usernode, sender: Node): Set[Long] = {
    Set[Long]()
  }

  override def gossipMessage(node: Usernode, info: Info, pid: Int) {
    saveInfo(node, info)
  }
}