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

trait GeneralProtocol extends CDProtocol with EDProtocol {

  val maxHops = Oracle.maxHops

  def nextCycle(node: Node, pid: Int) {}

  def shouldLookForNewNeighbor(un: Usernode): Boolean;

  def computeFanout(gossiper: Usernode, sender: Node): Set[Long];

  def initializeScoreList(un: Usernode, ids: Seq[Long]);

  def addToScoreList(un: Usernode, nid: Long);

  def canAcceptNewNeighbor(un: Usernode): Boolean

  def aboveBaseRank: Boolean = {
    Oracle.forwardProbability > Random.nextFloat
  }

  def newNodeSolving(id: Int) = {}
  
  def belowBaseRank(score: Float): Boolean = {
    val calced = Oracle.forwardProbability * Math.abs((-Oracle.FR_THRESHOLD - (-score)) / Oracle.FR_THRESHOLD)
    val random = Random.nextFloat
    val a = calced > random
    a
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
        gossipMessage(usernode, info, pid)
        info.sender.newMessages += 1
      case usernode: Usernode =>
        usernode.increaseScore(info.sender, 1)
        info.sender.repeatedMessages += 1
    }
  }

  def gossipMessage(node: Usernode, info: Info, pid: Int) {
    if (saveInfo(node, info)) {
      val linkable = Oracle.getLinkable(node)
      computeFanout(node, info.sender) map {
        id =>
          linkable.getNeighborById(id) match {
            case Some(peern) if peern.isUp =>
              // if (!peern.messageList.contains(info.value)) {
              sendInfo(node, peern, Info(info.value, node, info.hop + 1), pid)
            //}
            case _ => print("node not up")
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
        Oracle.saveHop(info)
      case _ => ???
    }

  }
}