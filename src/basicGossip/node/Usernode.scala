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

class Usernode(prefix: String) extends ModifiableNode(prefix) {

  var messageList: Array[Option[Info]] = Array.fill(BasicGossip.cycles)(None: Option[Info])
  var scoreList = Map[Long, Float]()

  var waitingConfirm = MutableList[Int]()
  var solvingChallenges = MutableList[WaitCycles]()

  def addChallenge(sender: Usernode) = solvingChallenges.+=(WaitCycles(100, sender))

  def solveChallenge = {
    solvingChallenges = solvingChallenges map {
      elem => elem.copy(remainingCycles = elem.remainingCycles - 1)
    }
  }

  def cleanSolvedChallenges = solvingChallenges = solvingChallenges.filterNot(_.remainingCycles == 0)

  def saveMessage(info: Info) = {
    messageList(info.value) = Some(info)
  }

  def receivedSolvedChallenge(newMember: Usernode) = {
    if (this.waitingConfirm.contains(newMember.getID) && canAcceptNewNeighbor) {
      // println("NODE: " + this.getID + " received solved challenge")
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

  def containsElem(value: Int): Boolean = messageList(value).isDefined

  def addToScoreList(nid: Long) {
    scoreList = scoreList.updated(nid, 0.4F)
  }

  def increaseScore(node: Node) {
    // used when we receive info from node
    val id = node.getID
    scoreList = scoreList match {
      case map if map.contains(id) =>
        scoreList.updated(id, Math.max((scoreList(id) + 0.05F), 1F))
      case map if !map.contains(id) && node.getID != 0 =>
        //println("Node: " + this.getID + " received info from non symmetric node " + node.getID)
        scoreList //.updated(id, 999)
      case _ => scoreList
    }
  }

  def decreaseScore(node: Node) {
    // used when we send info to node
    val id = node.getID
    scoreList = scoreList match {
      case map if map.contains(id) =>
        scoreList.updated(id, scoreList(id) - 0.05F)
      case _ =>
        scoreList.updated(id, -1)
    }
  }

  def initializeScoreList(ids: Seq[Long]) = {
    scoreList = Map(ids map {
      id => id -> 1F
    }: _*)
  }

  def getHpvProtocol: HyParViewJoinTest =
    this.getProtocol(HyParViewJoinTest.protocolID) match {
      case prot: HyParViewJoinTest => prot
    }

  //  def randomGossip(fanout: Int, sender: Node): List[Long] = {
  //    Oracle.peerAlgorithm match {
  //      case 0 =>
  //        val calculated = Random.shuffle(scoreList.filter { x => x._1 != 0 && x._1 != sender.getID && x._2 > -20 })
  //        calculated.take(fanout).toList.map(_._1)
  //        scoreList.filter { x => x._1 != 0 && x._2 > -15 && x._1 != sender.getID } map (_._1) toList
  //      case 1 =>
  //        scoreList.filter { x => x._1 != 0 && x._1 != sender.getID } map (_._1) toList
  //    }
  //  }

  def randomGossip(fanout: Int, sender: Node): Set[Long] =
    Oracle.peerAlgorithm match {
      case 0 =>
        val probability = Random.nextFloat
        scoreList.filter(x => x._1 != 0 && x._1 != sender.getID && x._2 > probability).keySet // diff freeRiders
      case 1 =>
        scoreList.filter(x => x._1 != 0).keySet
    }

  def dumpAltruistics =
    scoreList.filter(_._2 > 0.5).map(_._1) toList

  override def clone(): Object = {
    this.scoreList = Map[Long, Float]()
    this.messageList = Array.fill(BasicGossip.cycles)(None: Option[Info])
    this.waitingConfirm = new MutableList[Int]()
    this.solvingChallenges = new MutableList[WaitCycles]()
    super.clone()
  }

  def removeFromScoreList(id: Long) = {
    scoreList = scoreList.filterKeys(_ != id)
  }

  def freeRiders =
    scoreList.filter(_._2 < 0).map(_._1) toList

  def shouldLookForNewNeighbor: Boolean = freeRiders.size > 0 || canAcceptNewNeighbor
  def canAcceptNewNeighbor: Boolean = Oracle.getLinkable(this).neigh.size < Oracle.minWindow

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
