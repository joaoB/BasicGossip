package basicGossip.node

import scala.collection.mutable.BitSet
import scala.collection.mutable.MutableList
import NodeStatus.ACTIVE
import NodeStatus.NodeStatus
import NodeStatus.WAITING
import basicGossip.messages.Info
import basicGossip.messages.WaitCycles
import basicGossip.oracle.Oracle
import basicGossip.protocols.GeneralProtocol
import peersim.config.Configuration
import peersim.core.ModifiableNode
import peersim.core.Node
import scala.util.Random

object NodeStatus extends Enumeration {
  type NodeStatus = Value
  val ACTIVE, SOLVING, WAITING, DISCONNECTED = Value
}
case class Neighbor(score: Int, status: NodeStatus)

class Usernode(prefix: String) extends ModifiableNode(prefix) {
  //var messageList/*: Array[Option[Info]] */= Array.fill(BasicGossip.cycles)(None: Option[Info])

  var messageList = BitSet()

  var scoreList = Map[Long, Neighbor]()

  var newMessages = 0
  var repeatedMessages = 0

  var waitingConfirm = MutableList[Int]()
  //var solvingChallenges = MutableList[WaitCycles]()

  val scoreDelta = Configuration.getDouble("Usernode." + "SCORE_DELTA").toFloat

  def behaviorProtocol: GeneralProtocol = this.getProtocol(0) match {
    case protocol: GeneralProtocol => protocol
  }

  def isActive(n: NodeStatus) = {
    n == NodeStatus.ACTIVE
  }

  def newNodeSolving(id: Int) = {
    behaviorProtocol.newNodeSolving(this, id)
  }

  def addNewChallenge(node: Usernode) {
    val solving = scoreList.filter(_._2.status == NodeStatus.SOLVING)
    if (!solving.exists(_._1 == node.getID)) {
      val randomFactor = Random.nextInt(40)
      val time = Oracle.QUARANTINE - 40 + randomFactor
      val toAdd = Neighbor(time, NodeStatus.SOLVING)
      this.scoreList = this.scoreList.updated(node.getID, toAdd)
    }

  }

  def solveChallenge = {

    val solved = scoreList.filter(_._2.status == NodeStatus.SOLVING)

    solved.find { x => x._2.score > 0 } match {
      case Some(elem) => decreaseScore(Oracle.getNode(elem._1.toInt))
      case None =>
    }
  }

  def cleanSolvedChallenges = {
    println(scoreList)
    scoreList.filter(x => x._2.status == NodeStatus.SOLVING) map {
      elem => addToScoreList(elem._1)
    }
    println(scoreList)
  }

  def saveMessage(info: Info) = {
    messageList.+=(info.value)
  }

  def receivedSolvedChallenge(newMember: Usernode) = {
    val waiting = this.scoreList.filter(x => x._2.status == WAITING).map(_._1) toList

    if (waiting.contains(newMember.getID) && canAcceptNewNeighbor) {
      Oracle.getNode(newMember.getID.toInt).addToScoreList(this.getID)
      this.addToScoreList(newMember.getID)

    } else {
      if (waiting.contains(newMember.getID)) {

        val a = (this.scoreList.filter(_._1 != newMember.getID).map {
          x => (x._1, x._2.score)
        }).toList

        val min = a.minBy(_._2)._2

        val b = a.filter(_._2 == min).headOption

        b match {
          case Some(elem) =>
            val id = elem._1.toInt

            this.removeFromScoreList(id)
            Oracle.getNode(id).removeFromScoreList(this.getID)

            this.addToScoreList(newMember.getID)
            newMember.addToScoreList(this.getID)

          case None =>
        }

      }

    }
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
        val status = map(id).status
        scoreList.updated(id, Neighbor(newScore, status))
      case map =>
        val newScore = -1
        val status = map(id).status
        scoreList.updated(id, Neighbor(newScore, status))
    }
  }

  def initializeScoreList(ids: Seq[Long]) = {
    behaviorProtocol.initializeScoreList(this, ids)
  }

  override def clone(): Object = {
    this.scoreList = Map[Long, Neighbor]()
    this.messageList = BitSet()
    this.waitingConfirm = new MutableList[Int]()
    //this.solvingChallenges = new MutableList[WaitCycles]()
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
}
