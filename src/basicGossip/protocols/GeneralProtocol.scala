package basicGossip.protocols

import scala.util.Random
import basicGossip.messages.Info
import basicGossip.node.Usernode
import basicGossip.oracle.Oracle
import peersim.cdsim.CDProtocol
import peersim.config.FastConfig
import peersim.core.Node
import peersim.edsim.EDProtocol
import peersim.transport.Transport
import basicGossip.messages.ConfirmSolveChallenge
import basicGossip.node.Neighbor
import basicGossip.node.NodeStatus

object ProtocolName extends Enumeration {
  type ProtocolName = Value
  val ALT, FR = Value
}

trait GeneralProtocol extends CDProtocol with EDProtocol {
  import basicGossip.protocols.ProtocolName._

  val baseWin: Int
  val maxWin: Int

  val maxHops = Oracle.maxHops

  val protocolName: ProtocolName

  def nextCycle(node: Node, pid: Int) {}

  def shouldLookForNewNeighbor(un: Usernode): Boolean;

  def computeFanout(gossiper: Usernode, sender: Node): Set[Long];

  def initializeScoreList(un: Usernode, ids: Seq[Long]) = {
    un.scoreList = Map(ids map {
      id => id -> Neighbor(Oracle.baseRank.toInt, NodeStatus.ACTIVE)
    }: _*)
  }

  def addToScoreList(un: Usernode, nid: Long) {
    val neigh = Neighbor(Oracle.baseRank.toInt, NodeStatus.ACTIVE)
    un.scoreList = un.scoreList.updated(nid, neigh)
  }

  def canAcceptNewNeighbor(un: Usernode): Boolean

  def aboveBaseRank: Boolean = {
    Oracle.forwardProbability > Random.nextFloat
  }

  def newNodeSolving(un: Usernode, id: Int) = {
    val neigh = Neighbor(Oracle.baseRank.toInt, NodeStatus.WAITING)
    un.scoreList = un.scoreList.updated(id, neigh)
  }

  def belowBaseRank(score: Float): Boolean = {
    val calced = Oracle.forwardProbability * Math.abs((-Oracle.FR_THRESHOLD - (-score)) / Oracle.FR_THRESHOLD)
    calced > Random.nextFloat
  }

  def processEvent(node: Node, pid: Int, event: Object) = {
    event match {
      case info: Info => {
        processInfo(node, pid, info)
      }
      case _ => ???
    }
  }

  def processInfo(node: Node, pid: Int, info: Info) {
     
    node match {
      case usernode: Usernode if !usernode.containsElem(info.value) =>
        Oracle.saveMaxHopInfo(info)
        Oracle.saveHop(info)
        gossipMessage(usernode, info, pid)
        info.sender.newMessages += 1

      case usernode: Usernode =>
        usernode.increaseScore(info.sender, 1)
        info.sender.repeatedMessages += 1
    }
  }

  def gossipMessage(node: Usernode, info: Info, pid: Int) {
    if (saveInfo(node, info)) {
      //val linkable = Oracle.getLinkable(node)
      computeFanout(node, info.sender) map {
        id =>
          Oracle.getNode(id.toInt) match {
            case peern if peern.isUp =>
              sendInfo(node, peern, Info(info.value, node, info.hop + 1), pid)
            case _ => //print("node not up")
          }
      }
    }
  }

  def saveInfo(node: Usernode, info: Info): Boolean = {
    node.increaseScore(info.sender, 1)
    node.saveMessage(info)
    true
  }

  def sendToRandom(node: Usernode, info: Info, pid: Int) = {
    node.getProtocol(FastConfig.getLinkable(pid)) match {
      case link: Link =>
        val peern = link.getNeighbor(Random.nextInt(link.degree))
        if (peern.isUp()) {
          sendInfo(node, peern, Info(info.value, node, info.hop + 1), pid)
        }
    }
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