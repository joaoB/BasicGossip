package basicGossip.node

import scala.collection.mutable.BitSet
import scala.collection.mutable.MutableList
import basicGossip.messages.Info
import basicGossip.messages.WaitCycles
import basicGossip.oracle.Oracle
import basicGossip.protocols.ChallengeSolver
import basicGossip.protocols.GeneralProtocol
import basicGossip.protocols.SearchNewNeighbor
import peersim.config.Configuration
import peersim.core.ModifiableNode
import peersim.core.Network
import peersim.core.Node
import basicGossip.messages.WaitCycles
import scala.util.Random

object NodeStatus extends Enumeration {
  type NodeStatus = Value
  val ACTIVE, SOLVING, WAITING, DISCONNECTED = Value
}
import NodeStatus._
case class Neighbor(score: Int, status: NodeStatus)

class Usernode(prefix: String) extends ModifiableNode(prefix) {
  var messageList = BitSet()

  var scoreList = Map[Long, Neighbor]()

  var newMessages = 0
  var repeatedMessages = 0

  //var waitingConfirm = MutableList[Int]()
  var solvingChallenges = Map[Long, Int]()

  val scoreDelta = Configuration.getDouble("Usernode." + "SCORE_DELTA").toFloat

  def behaviorProtocol: GeneralProtocol = this.getProtocol(0) match {
    case protocol: GeneralProtocol => protocol
  }

  def dropConnections = behaviorProtocol.dropConnections(this)

  def isActive(n: NodeStatus) = {
    n == NodeStatus.ACTIVE
  }

  def newNodeSolving(id: Int) = {
    behaviorProtocol.newNodeSolving(this, id)
  }

  def addNewChallenge(node: Usernode) {
    //Oracle.altruisticChallanges += 1
    val neigh = Neighbor(Oracle.baseRank.toInt, NodeStatus.SOLVING)
    this.scoreList = this.scoreList.updated(node.getID, neigh)

    val random = new Random
    val time = Oracle.QUARANTINE - 10 + random.nextInt(20)

    //    if (Oracle.currentPackage >= 5500)
    //      if (Oracle.altruistics.contains(this.getID)) {
    //        Oracle.altruisticChallanges += 1
    //      } else {
    //        println("WIL NOT COUNT")
    //      }

    Oracle.incChallenges(this)
    //solvingChallenges.+=(WaitCycles(time, node))
    solvingChallenges = solvingChallenges.updated(node.getID, time)
  }

  def solveChallenge = {
    solvingChallenges.find {
      elem => elem._2 != 0
    } match {
      case Some(elem) => solvingChallenges = solvingChallenges.updated(elem._1, elem._2 - 1)
      case None => 
    }
  }

 /* def solveChallenge = {
    val solved = solvingChallenges.find { x => x.remainingCycles > 0 } match {
      case Some(elem) => MutableList(elem.copy(remainingCycles = elem.remainingCycles - 1))
      case None => MutableList[WaitCycles]()
    }

    solvingChallenges = solved.headOption match {
      case Some(elem) => solved ++ solvingChallenges.filter { x => x.sender.getID != elem.sender.getID }
      case _ => solvingChallenges
    }
  }*/

  def cleanSolvedChallenges = solvingChallenges = solvingChallenges.filterNot(_._2 <= 0)

  def saveMessage(info: Info) = {
    messageList.+=(info.value)
  }

  def receivedSolvedChallenge(newMember: Usernode): Unit = {
    val waiting = this.scoreList.filter(x => x._2.status == WAITING).map(_._1) toList

    if (!newMember.canAcceptNewNeighbor) {
      this.removeFromScoreList(newMember.getID)
      newMember.removeFromScoreList(this.getID)
      //println("A " + this.scoreList.filter(_._2.status == ACTIVE).size)
      //println("B " + newMember.scoreList.filter(_._2.status == ACTIVE).size)
      //println("%%%%")
      return

    }

    if (waiting.contains(newMember.getID) && canAcceptNewNeighbor) {
      Oracle.getNode(newMember.getID.toInt).addToScoreList(this.getID)
      this.addToScoreList(newMember.getID)
    } else {
      if (waiting.contains(newMember.getID)) {
        val elem = (this.scoreList.filter(_._2.status == ACTIVE).map {
          x => (x._1, x._2.score)
        }).toList.sortBy(_._2).head

        this.removeFromScoreList(elem._1)
        Oracle.getNode(elem._1.toInt).solvingChallenges = Oracle.getNode(elem._1.toInt).solvingChallenges.filter(_._1 != this.getID)
        Oracle.getNode(elem._1.toInt).removeFromScoreList(this.getID)

        this.addToScoreList(newMember.getID)
        newMember.addToScoreList(this.getID)
      }
    }

    //println("A " + this.scoreList.filter(_._2.status == ACTIVE).size)
    //println("B " + newMember.scoreList.filter(_._2.status == ACTIVE).size)
    //println("-------------------------------------------")
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

  override def clone(): Object = {
    this.scoreList = Map[Long, Neighbor]()
    this.messageList = BitSet()
    this.solvingChallenges = Map[Long, Int]()
    super.clone()
  }

  def removeFromScoreList(id: Long) = {
    scoreList = scoreList.filterKeys(_ != id)
  }

  def freeRiders = {
    scoreList.filter(x => x._2.score <= Oracle.FR_THRESHOLD && x._2.status == ACTIVE).map(_._1) toList
  }

  def shouldLookForNewNeighbor: Boolean = {
    behaviorProtocol.shouldLookForNewNeighbor(this)
  }

  def canAcceptNewNeighbor: Boolean = behaviorProtocol.canAcceptNewNeighbor(this)

}
