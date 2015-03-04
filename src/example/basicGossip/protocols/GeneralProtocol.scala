package example.basicGossip.protocols

import example.basicGossip.Info
import example.basicGossip.Usernode
import example.basicGossip.oracle.Oracle
import peersim.cdsim.CDProtocol
import peersim.core.Node
import peersim.edsim.EDProtocol
import peersim.transport.Transport
import peersim.config.FastConfig
import scala.util.Random

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
  
  def sendToRandom(node: Usernode, info: Info, pid: Int) = {
    node.getProtocol(FastConfig.getLinkable(pid)) match {
      case link: Link =>
        val peern = link.getNeighbor(Random.nextInt(link.degree))
        if (peern.isUp()) {
          node.getProtocol(FastConfig.getTransport(pid)) match {
            case trans: Transport =>
              sendInfo(trans, node, peern, Info(info.value, node, info.hop + 1), pid)
          }
        }
    }
  }

  def sendInfo(trans: Transport, node: Usernode, peern: Node, info: Info, pid: Int) {
    Oracle.incSentMessages
    trans.send(node, peern, Info(info.value, node, info.hop + 1), pid)
    node.decreaseScore(peern)
  }
}