package basicGossip.node

import scala.collection.mutable.MutableList
import scala.util.Random
import basicGossip.protocols.BasicGossip
import hyparview.HyParViewJoinTest
import peersim.core.ModifiableNode
import peersim.core.Node
import basicGossip.messages.Info
import basicGossip.oracle.Oracle
import peersim.core.Network
import basicGossip.messages.WaitCycles
import peersim.config.Configuration
import scala.collection.mutable.BitSet
import basicGossip.protocols.AltruisticProtocol
import basicGossip.protocols.FRProtocol
import basicGossip.protocols.GeneralProtocol

object NodeStatus extends Enumeration {
  type NodeStatus = Value
  val ACTIVE, SOLVING, WAITING, DISCONNECTED = Value
}

import NodeStatus._
case class Neighbor(score: Int, status: NodeStatus)

class Usernode(prefix: String) extends ModifiableNode(prefix) {
  //var messageList/*: Array[Option[Info]] */= Array.fill(BasicGossip.cycles)(None: Option[Info])

  var messageList = BitSet()

  var scoreList = Map[Long, Neighbor]()

  var newMessages = 0
  var repeatedMessages = 0

  var waitingConfirm = MutableList[Int]()
  var solvingChallenges = MutableList[WaitCycles]()

  val scoreDelta = Configuration.getDouble("Usernode." + "SCORE_DELTA").toFloat

  def behaviorProtocol: GeneralProtocol = this.getProtocol(0) match {
    case protocol: GeneralProtocol => protocol
  }

  def isActive(n: NodeStatus) = {
    n == NodeStatus.ACTIVE
  }

  def newNodeSolving(id: Long) = {}

  def addChallenge(sender: Usernode) = {
    if (!solvingChallenges.exists { x => x.sender.getID == sender.getID }) {

      //println("Node : " + this.getID + " adding " + sender.getID)
      solvingChallenges.+=(WaitCycles(Oracle.QUARANTINE, sender))
      true
    } else {
      false
    }
  }

  def solveChallenge = {
    solvingChallenges = solvingChallenges map {
      elem =>
        elem.copy(remainingCycles = elem.remainingCycles - 1)
    }
  }

  def cleanSolvedChallenges = solvingChallenges = solvingChallenges.filterNot(_.remainingCycles == 0)

  def saveMessage(info: Info) = {
    messageList.+=(info.value)
  }

  def receivedSolvedChallenge(newMember: Usernode) = {
    if (this.waitingConfirm.contains(newMember.getID) && canAcceptNewNeighbor) {
      val link = Oracle.getLinkable(this)
      Oracle.getLinkable(newMember.getID.toInt).addNeighbor(this)
      link.addNeighbor(newMember)

      Oracle.getNode(newMember.getID.toInt).addToScoreList(this.getID)
      this.addToScoreList(newMember.getID)
    } else {
      Oracle.nodeHpvProtocol(this.getID.toInt)._2.disconnect(Network.get(newMember.getID.toInt))
      Oracle.nodeHpvProtocol(newMember.getID.toInt)._2.disconnect(Network.get(this.getID.toInt))

    }
    waitingConfirm = waitingConfirm.filterNot(_ == newMember.getID)

  }

  def containsElem(value: Int): Boolean = messageList(value)

  def addToScoreList(nid: Long) {
    behaviorProtocol.addToScoreList(this, nid)
  }

  def increaseScore(node: Node, score: Float) {
    // used when we receive info from node
    val id = node.getID
    scoreList = scoreList match {
      case map if map.contains(id) =>
        val newScore = map(id).score + 1
        scoreList.updated(id, Neighbor(newScore, ACTIVE))
      case _ => scoreList
    }
  }

  def increaseScore(node: Node) {
    increaseScore(node, scoreDelta)
  }

  def decreaseScore(node: Node) {
    // used when we send info to node
    val id = node.getID
    scoreList = scoreList match {
      case map if map.contains(id) =>
        val newScore = map(id).score - 1
        scoreList.updated(id, Neighbor(newScore, ACTIVE))
      case _ =>
        val newScore = -1
        scoreList.updated(id, Neighbor(newScore, ACTIVE))
    }
  }

  def initializeScoreList(ids: Seq[Long]) = {
    behaviorProtocol.initializeScoreList(this, ids)
  }

  def getHpvProtocol: HyParViewJoinTest =
    this.getProtocol(HyParViewJoinTest.protocolID) match {
      case prot: HyParViewJoinTest => prot
    }

  override def clone(): Object = {
    this.scoreList = Map[Long, Neighbor]()
    this.messageList = BitSet()
    this.waitingConfirm = new MutableList[Int]()
    this.solvingChallenges = new MutableList[WaitCycles]()
    super.clone()
  }

  def removeFromScoreList(id: Long) = {
    scoreList = scoreList.filterKeys(_ != id)
  }

  def freeRiders = {
    scoreList.filter(_._2.score <= Oracle.FR_THRESHOLD).map(_._1) toList
  }

  def shouldLookForNewNeighbor: Boolean = {
    behaviorProtocol.shouldLookForNewNeighbor(this)
  }

  def canAcceptNewNeighbor: Boolean = behaviorProtocol.canAcceptNewNeighbor(this)

  def addWaitingConfirm(id: Int) =
    if (!waitingConfirm.contains(id)) waitingConfirm += id

  //  def receiveSolvedChallenge(id: Int) =
  //    if (waitingConfirm.contains(id)) {
  //      waitingConfirm = waitingConfirm.filter(_ != id)
  //      addToScoreList(id)
  //    }

  //
  //  def addNewNeighbor(id: Int): Boolean =
  //    if (isWaitingConnection) {
  //      scoreList = scoreList.updated(id, 0)
  //      Oracle.getLinkable(this).addNeighbor(Network.get(id))
  //      isWaitingConnection = false
  //      true
  //    } else false
  //
  //  def receivePositiveConfirm(nid: Int) = 
  //    if (waitingConfirm.contains(nid)) {
  //      scoreList = scoreList.updated(nid, 0)
  //      Oracle.getLinkable(this).addNeighbor(Network.get(nid))
  //    }

}
