package basicGossip.protocols

import basicGossip.messages.Info
import basicGossip.node.Usernode



class FRProtocol(name: String) extends ThreeFaseGossip(name) with GeneralProtocol  {

  override def gossipMessage(node: Usernode, info: Info, pid: Int) {
    saveInfo(node, info)
  }

}