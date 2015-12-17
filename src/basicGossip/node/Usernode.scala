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
import utils.RingBuffer
import basicGossip.protocols.Lifting
import basicGossip.protocols.GeneralProtocol.GeneralProtocol

object NodeStatus extends Enumeration {
  type NodeStatus = Value
  val ACTIVE, SOLVING, WAITING, DISCONNECTED = Value
}
import NodeStatus._
case class Neighbor(score: Float, status: NodeStatus)

class Usernode(prefix: String) extends ModifiableNode(prefix) {

  var messageList = BitSet()

  var scoreList = Map[Long, Neighbor]()

  var newMessages = 0
  var repeatedMessages = 0

  var sentMessages = 0
  
  
  var waiting = BitSet()
  
  var ring: RingBuffer[Double] = new RingBuffer(20)
  for (i <- 0 until 20) ring.+=(0)
  def avgHops = ring.sum / ring.size

  var solvingChallenges = Map[Long, Int]()
  var proposals = Map[Int, MutableList[Info]]()
  //var proposals = Map[Int, MutableList[Long]]()

  val scoreDelta = Configuration.getDouble("Usernode." + "SCORE_DELTA").toFloat

  def kickFreeRiders = behaviorProtocol.kickFreeRiders(this)

  def behaviorProtocol: GeneralProtocol = this.getProtocol(0) match {
    case protocol: GeneralProtocol => protocol
  }

  def blackList = behaviorProtocol.blackList
  
  def dropConnections = behaviorProtocol.dropConnections(this)

  def requestProposals = {
    proposals.filterKeys { x => x < Oracle.currentPackage && !messageList(x) } map {
      elems =>
        val a = scoreList.filter(elems._2.map(_.sender.getID) contains _._1)
        if (!a.isEmpty) {
          val minID = a.minBy(_._2.score)._1.toInt
          elems._2.find { x => x.sender.getID == minID } match {
            case Some(info) if scoreList(minID).score > -Oracle.FR_THRESHOLD - 3 =>
            case Some(info) =>
              behaviorProtocol.sendInfo(Oracle.getNode(minID), this, Info(elems._1, Oracle.getNode(minID), info.hop + 1), 0)
            case None =>
          }
        }
    }
    proposals = proposals.filterNot(x => x._1 < Oracle.currentPackage)
  }

  def newProposal(info: Info) =
    proposals = proposals.get(info.value) match {
      //case Some(elem) => proposals.updated(info.value, elem.+=(info.sender.getID))
      //case None => proposals.updated(info.value, MutableList(info.sender.getID))
      case Some(elem) => proposals.updated(info.value, elem.+=(info))
      case None => proposals.updated(info.value, MutableList(info))

    }

  def newNodeSolving(id: Int) = {
    behaviorProtocol.newNodeSolving(this, id)
  }

  def addNewChallenge(node: Usernode) {
    val neigh = Neighbor(Oracle.baseRank.toInt, NodeStatus.SOLVING)
    this.scoreList = this.scoreList.updated(node.getID, neigh)

    val random = new Random
    val time = Oracle.QUARANTINE - 10 + random.nextInt(20)
    Oracle.incChallenges(this)
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

  def cleanSolvedChallenges = solvingChallenges = solvingChallenges.filterNot(_._2 <= 0)

  def updateAvg(info: Info) = {
    ring.+=(ring.next * 0.7 + info.hop * 0.3)
  }

  def saveMessage(info: Info) = {
    messageList.+=(info.value)

  }

  def receivedSolvedChallenge(newMember: Usernode): Unit = {
    val waiting = this.scoreList.filter(x => x._2.status == WAITING).map(_._1) toList

    if (!newMember.canAcceptNewNeighbor) {
      this.removeFromScoreList(newMember.getID)
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
        val newScore = map(id).score + score
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
    this.ring = new RingBuffer(20)
    super.clone()
  }

  def removeFromScoreList(id: Long) = {
    scoreList = scoreList.filterKeys(_ != id)
  }

  def freeRiders = {
    behaviorProtocol.freeRiders(this)
    //scoreList.filter(x => x._2.score <= Oracle.FR_THRESHOLD && x._2.status == ACTIVE).map(_._1) toList
  }

  def shouldLookForNewNeighbor: Boolean = {
    behaviorProtocol.shouldLookForNewNeighbor(this)
  }

  def canAcceptNewNeighbor: Boolean = behaviorProtocol.canAcceptNewNeighbor(this)

}
