package basicGossip.protocols

import basicGossip.messages.Info
import basicGossip.node.Usernode
import basicGossip.oracle.Oracle
import basicGossip.protocols.dissemination.FRDisseminator
import basicGossip.protocols.GeneralProtocol.ProtocolName

class FRProtocol(name: String) extends AltruisticProtocol(name) {
  override val protocolName = ProtocolName.FR
  override val disseminator = FRDisseminator

  override def gossipMessage(node: Usernode, info: Info, pid: Int) {
    saveInfo(node, info)
  }

  override def swapToLifting(un: Usernode) = {
    //rationals do not swap
  }

}