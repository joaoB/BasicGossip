package basicGossip.protocols

import basicGossip.messages.Info
import basicGossip.node.Usernode



class FRProtocol(name: String) extends GeneralProtocol(name) {

  override def sendMessage(node: Usernode, info: Info, pid: Int) {
    saveInfo(node, info)
  }

}