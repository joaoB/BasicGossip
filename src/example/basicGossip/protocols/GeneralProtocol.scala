package example.basicGossip.protocols

import peersim.edsim.EDProtocol
import peersim.cdsim.CDProtocol
import peersim.core.Node
import example.basicGossip.Info
import example.basicGossip.Usernode
import example.basicGossip.oracle.Oracle

abstract class GeneralProtocol(name: String) extends CDProtocol with EDProtocol {

  def nextCycle(node: Node, pid: Int) {}

  def processEvent(node: Node, pid: Int, event: Object) = {
    event match {
      case info: Info => node match {
        case usernode: Usernode if !usernode.containsElem(info.value) =>
          Oracle.saveMaxHopInfo(info)
          sendMessage(usernode, info, pid)
        case usernode: Usernode => usernode.increaseScore(info.sender)
        case _ => ???
      }
      case _ => ???
    }
  }

  def sendMessage(node: Usernode, info: Info, pid: Int)

  def saveInfo(node: Usernode, info: Info): Boolean = {
    node.increaseScore(info.sender)
    if (!node.containsElem(info.value)) {
      node.saveMessage(info.value)
      true
    }
    false
  }
}