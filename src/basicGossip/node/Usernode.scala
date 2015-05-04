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

class Usernode(prefix: String) extends ModifiableNode(prefix) {

  //var messageList/*: Array[Option[Info]] */= Array.fill(BasicGossip.cycles)(None: Option[Info])
  var messageList = BitSet()

  var scoreList = Map[Long, Float]()

  var waitingConfirm = MutableList[Int]()
  var solvingChallenges = MutableList[WaitCycles]()

  val scoreDelta = Configuration.getDouble("Usernode." + "SCORE_DELTA").toFloat
  val FR_THRESHOLD = Configuration.getDouble("Usernode." + "FR_THRESHOLD").toFloat

  def behaviorProtocol: GeneralProtocol = this.getProtocol(0) match {
    case protocol: GeneralProtocol => protocol
  }

  def addChallenge(sender: Usernode) = {
    if (!solvingChallenges.exists { x => x.sender.getID == sender.getID }) {
      solvingChallenges.+=(WaitCycles(20, sender))
      true
    } else {
      false
    }
  }

  def solveChallenge = {
    solvingChallenges = solvingChallenges map {
      elem => elem.copy(remainingCycles = elem.remainingCycles - 1)
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
    }
    waitingConfirm = waitingConfirm.filterNot(_ == newMember.getID)

  }

  def containsElem(value: Int): Boolean = messageList(value)

  def addToScoreList(nid: Long) {
    behaviorProtocol.addToScoreList(this, nid)
  }

  def increaseScore(node: Node) {
    // used when we receive info from node
    val id = node.getID
    scoreList = scoreList match {
      case map if map.contains(id) =>
        scoreList.updated(id, scoreList(id) + scoreDelta)
      case _ => scoreList
    }
  }

  def decreaseScore(node: Node) {
    // used when we send info to node
    val id = node.getID
    scoreList = scoreList match {
      case map if map.contains(id) =>
        scoreList.updated(id, scoreList(id) - scoreDelta)
      case _ =>
        scoreList.updated(id, -1)
    }
  }

  def initializeScoreList(ids: Seq[Long]) = {
    behaviorProtocol.initializeScoreList(this, ids)
  }

  def getHpvProtocol: HyParViewJoinTest =
    this.getProtocol(HyParViewJoinTest.protocolID) match {
      case prot: HyParViewJoinTest => prot
    }

  //  def randomGossip(fanout: Int, sender: Node): Set[Long] =
  //    Oracle.peerAlgorithm match {
  //      case 0 =>
  //        val probability = Random.nextFloat
  //        scoreList.filter(x => x._1 != 0 && x._1 != sender.getID && x._2 > probability).keySet
  //      case 1 =>
  //        scoreList.filter(x => x._1 != 0).keySet
  //    }

  def dumpAltruistics =
    scoreList.filter(_._2 > 0.5).map(_._1) toList

  override def clone(): Object = {
    this.scoreList = Map[Long, Float]()
    // this.messageList = Array.fill(BasicGossip.cycles)(None: Option[Info])
    this.messageList = BitSet()
    this.waitingConfirm = new MutableList[Int]()
    this.solvingChallenges = new MutableList[WaitCycles]()
    super.clone()
  }

  def removeFromScoreList(id: Long) = {
    scoreList = scoreList.filterKeys(_ != id)
  }

  def freeRiders = {
    scoreList.filter(_._2 <= FR_THRESHOLD).map(_._1) toList
  }

  def shouldLookForNewNeighbor: Boolean = {
    behaviorProtocol.shouldLookForNewNeighbor(this)
  }

  def canAcceptNewNeighbor: Boolean = behaviorProtocol.canAcceptNewNeighbor(this)

  def altruisticsNeighbors: List[Long] = scoreList.filter(_._2 > 0.5).map(_._1) toList

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
