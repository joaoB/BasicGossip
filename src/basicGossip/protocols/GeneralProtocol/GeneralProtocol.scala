package basicGossip.protocols.GeneralProtocol

import basicGossip.messages.Info
import basicGossip.node.Usernode
import basicGossip.oracle.Oracle
import basicGossip.protocols.ScoreListManager
import basicGossip.protocols.dissemination.Disseminator
import peersim.cdsim.CDProtocol
import peersim.config.FastConfig
import peersim.core.Node
import peersim.edsim.EDProtocol
import peersim.transport.Transport

object ProtocolName extends Enumeration {
  type ProtocolName = Value
  val ALT, FR, LIFTING = Value
}

trait GeneralProtocol extends CDProtocol with EDProtocol with ScoreListManager {
  import basicGossip.protocols.GeneralProtocol.ProtocolName._

  val baseWin: Int
  val maxWin: Int
  val disseminator: Disseminator
  val maxHops = Oracle.maxHops

  val protocolName: ProtocolName

  def kickFreeRiders(un: Usernode): Unit

  def freeRiders(un: Usernode): List[Long]
  
  def nextCycle(node: Node, pid: Int) {}

  def shouldLookForNewNeighbor(un: Usernode): Boolean

  def blackList: scala.collection.mutable.Set[Long]
  
  def computeFanout(gossiper: Usernode, sender: Node, info: Info): Set[Long] = disseminator.computeFanout(gossiper, sender, info)
  def computeFanout(gossiper: Usernode, sender: Node): Set[Long] = disseminator.computeFanout(gossiper, sender)

  def canAcceptNewNeighbor(un: Usernode): Boolean

  def processEvent(node: Node, pid: Int, event: Object) = {
    event match {
      case info: Info => {
        processInfo(Oracle.getNode(node.getID.toInt), pid, info)
      }
      case _ => ???
    }
  }

  def swapToLifting(un: Usernode)

  def processInfo(usernode: Usernode, pid: Int, info: Info) {
    if (!usernode.containsElem(info.value)) {
      usernode.updateAvg(info)
      Oracle.saveMaxHopInfo(info)
      Oracle.saveHop(info)
      gossipMessage(usernode, info, pid)
      info.sender.newMessages += 1
    } else {
      usernode.increaseScore(info.sender, 1)
      info.sender.repeatedMessages += 1
    }
  }

  def gossipMessage(node: Usernode, info: Info, pid: Int) {
    if (saveInfo(node, info)) {
      val fanout = computeFanout(node, info.sender, info)
      fanout map {
        id =>
          Oracle.getNode(id.toInt) match {
            case peern if peern.isUp =>
              //println(peern.proposals)
              if (!peern.messageList.contains(info.value))
                //peern.newProposal(Info(info.value, node, info.hop + 1))
                sendInfo(node, peern, Info(info.value, node, info.hop + 1), pid)
            case _ =>
          }
      }
    }
  }

  def saveInfo(node: Usernode, info: Info): Boolean = {
    node.increaseScore(info.sender)
    node.saveMessage(info)
    true
  }

  def sendInfo(node: Usernode, peern: Node, info: Info, pid: Int) {
    node.getProtocol(FastConfig.getTransport(pid)) match {
      case trans: Transport =>
        Oracle.incSentMessages(node)
        trans.send(node, peern, info, pid)
        node.decreaseScore(peern)
      case _ => ???
    }

  }

  def dropConnections(un: Usernode) {
    val size = un.scoreList.size
    un.scoreList.take(size - Oracle.RACIONAL_MAX_CONNECTIONS) map {
      elem =>
        val id = elem._1.toInt
        un.removeFromScoreList(id)
        Oracle.getNode(id).removeFromScoreList(un.getID)
    }
  }

}