package basicGossip.protocols

import basicGossip.messages.Info
import basicGossip.node.Usernode
import basicGossip.oracle.Oracle

class FRProtocol(name: String) extends AltruisticProtocol(name) with GeneralProtocol {

  override def gossipMessage(node: Usernode, info: Info, pid: Int) {
    saveInfo(node, info)
  }

}