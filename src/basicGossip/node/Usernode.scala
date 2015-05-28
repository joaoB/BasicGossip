package basicGossip.node

import scala.collection.mutable.BitSet
import scala.collection.mutable.MutableList
import basicGossip.messages.Info
import basicGossip.messages.WaitCycles
import basicGossip.oracle.Oracle
import basicGossip.protocols.ChallengeSolver
import basicGossip.protocols.GeneralProtocol
import basicGossip.protocols.SearchNewNeighbor
import hyparview.HyParViewJoinTest
import peersim.config.Configuration
import peersim.core.ModifiableNode
import peersim.core.Network
import peersim.core.Node

import basicGossip.messages.WaitCycles

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

  def newNodeSolving(id: Int) = {
    behaviorProtocol.newNodeSolving(this, id)
  }

  def addNewChallenge(node: Usernode) {
    Oracle.altruisticChallanges += 1
    val neigh = Neighbor(Oracle.baseRank.toInt, NodeStatus.SOLVING)
    this.scoreList = this.scoreList.updated(node.getID, neigh)
    this.addChallenge(node)
  }

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
    // println("NODE: " + this.getID)
    //println("SOLVE CHALLENGES A " + solvingChallenges.map(x => (x.remainingCycles,x.sender.getID)))

    val solved = solvingChallenges.find { x => x.remainingCycles > 0 } match {
      case Some(elem) => MutableList(elem.copy(remainingCycles = elem.remainingCycles - 1))
      case None => MutableList[WaitCycles]()
    }

    solvingChallenges = solved.headOption match {
      case Some(elem) => solved ++ solvingChallenges.filter { x => x.sender.getID != elem.sender.getID }
      case _ => solvingChallenges
    }
    //println("SOLVE CHALLENGES B " + solvingChallenges.map(x => (x.remainingCycles,x.sender.getID)))
    //println("--------------------------------------")
    //    val tail = solvingChallenges.drop(1)
    //
    //    val head = solvingChallenges.headOption match {
    //      case Some(elem) => MutableList(elem.copy(remainingCycles = elem.remainingCycles - 1))
    //      case _ => MutableList[WaitCycles]()
    //    }
    //    solvingChallenges = head ++ tail

  }

  def cleanSolvedChallenges = solvingChallenges = solvingChallenges.filterNot(_.remainingCycles <= 0)

  def saveMessage(info: Info) = {
    messageList.+=(info.value)
  }

  def receivedSolvedChallenge(newMember: Usernode) = {
    val waiting = this.scoreList.filter(x => x._2.status == WAITING).map(_._1) toList

    //if (this.waitingConfirm.contains(newMember.getID) && canAcceptNewNeighbor) {
    if (waiting.contains(newMember.getID) && canAcceptNewNeighbor) {

      //  val link = Oracle.getLinkable(this)
      // Oracle.getLinkable(newMember.getID.toInt).addNeighbor(this)
      // link.addNeighbor(newMember)
      
      //println("received puzzle and will accept " + this.getID + " -> " + newMember.getID)

      Oracle.getNode(newMember.getID.toInt).addToScoreList(this.getID)
      this.addToScoreList(newMember.getID)
    } else {
      if (waiting.contains(newMember.getID)) {

        val a = (this.scoreList.map {
          x => (x._1, x._2.score)
        }).toList.sortBy(_._2).headOption

        //better remove from solving

        a match {
          case Some(elem) =>
            this.removeFromScoreList(elem._1)
            Oracle.getNode(elem._1.toInt).removeFromScoreList(this.getID)
            this.addToScoreList(newMember.getID)
            newMember.addToScoreList(this.getID)
          case None =>
            println(this.scoreList)
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
