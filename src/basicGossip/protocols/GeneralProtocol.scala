package basicGossip.protocols

import scala.util.Random
import peersim.cdsim.CDProtocol
import peersim.config.FastConfig
import peersim.core.Node
import peersim.edsim.EDProtocol
import peersim.transport.Transport
import basicGossip.messages.Info
import basicGossip.node.Usernode
import basicGossip.oracle.Oracle
import basicGossip.messages.Propose
import basicGossip.messages.Request

trait GeneralProtocol extends CDProtocol with EDProtocol {

  val maxHops = Oracle.maxHops

  def nextCycle(node: Node, pid: Int) {}

  def processEvent(node: Node, pid: Int, event: Object) = {
    event match {
      case info: Info => processInfo(node, pid, info)
      case _ => ???
    }
  }

  def processInfo(node: Node, pid: Int, info: Info) {
    node match {
      case usernode: Usernode if !usernode.containsElem(info.value) =>
        Oracle.saveMaxHopInfo(info)
        gossipMessage(usernode, info, pid)
      case usernode: Usernode => usernode.increaseScore(info.sender)
      case _ => ???
    }
  }

  def gossipMessage(node: Usernode, info: Info, pid: Int)

  def saveInfo(node: Usernode, info: Info): Boolean = {
    node.increaseScore(info.sender)
    if (!node.containsElem(info.value)) {
      node.saveMessage(info)
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
    trans.send(node, peern, info, pid)
    node.decreaseScore(peern)
    Oracle.saveHop(info)
  }
  
  def sendSimpleMessage(sender: Usernode, receiver: Usernode, message : Any, pid: Int){
    sender.getProtocol(FastConfig.getTransport(pid)) match {
      case trans: Transport => trans.send(sender, receiver, message, pid)
      case _ => //dont know how to send this message
    }
  }
}