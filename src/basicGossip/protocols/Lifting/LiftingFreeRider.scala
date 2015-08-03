package basicGossip.protocols.Lifting

import basicGossip.messages.Info
import basicGossip.node.Usernode
import basicGossip.oracle.Oracle
import basicGossip.protocols.GeneralProtocol.Heavyweight
import basicGossip.protocols.GeneralProtocol.ProtocolName

class LiftingFreeRider(name: String) extends Heavyweight(name) {

  override val protocolName = ProtocolName.FR
  
  override def gossipMessage(node: Usernode, info: Info, pid: Int) {
    saveInfo(node, info)
    historySent.clear
  }

}