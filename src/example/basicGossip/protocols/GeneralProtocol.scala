package example.basicGossip.protocols

import peersim.edsim.EDProtocol
import peersim.cdsim.CDProtocol
import peersim.core.Node
import example.basicGossip.Info
import example.basicGossip.Usernode

abstract class GeneralProtocol(name: String) extends CDProtocol with EDProtocol {

  def nextCycle(node: Node, pid: Int) {}

  def processEvent(node: Node, pid: Int, event: Object) = {
    event match {
      case info: Info => node match {
        case usernode: Usernode if !usernode.containsElem(info.value) =>
          usernode.increaseScore(info.sender)
          sendMessage(usernode, info, pid)
        case node: Usernode =>
        case _ => ???
      }
      case _ => ???
    }
  }

  def sendMessage(node: Usernode, info: Info, pid: Int)

  def saveInfo(node: Usernode, value: Int): Boolean = {
    if (!node.containsElem(value)) {
      node.saveMessage(value)
      true
    }
    false
  }
}