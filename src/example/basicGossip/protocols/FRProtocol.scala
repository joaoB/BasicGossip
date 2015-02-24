package example.basicGossip.protocols

import example.basicGossip.Usernode
import example.basicGossip.Info

class FRProtocol(name: String) extends GeneralProtocol(name) {

  override def sendMessage(node: Usernode, info: Info, pid: Int) {
    saveInfo(node, info)
  }

}