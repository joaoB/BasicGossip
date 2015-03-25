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

class Usernode(prefix: String) extends ModifiableNode(prefix) {

  var messageList: Array[Option[Info]] = Array.fill(BasicGossip.cycles)(None: Option[Info])
  var scoreList = Map[Long, Float]()
  var waitingConfirm = MutableList[Int]()
  var isWaitingConnection = false

  def saveMessage(info: Info) = {
    messageList(info.value) = Some(info)
  }

  def containsElem(value: Int): Boolean = messageList(value).isDefined

  def increaseScore(node: Node) {
    // used when we receive info from node
    val id = node.getID
    scoreList = scoreList match {
      case map if map.contains(id) =>
        scoreList.updated(id, scoreList(id) + 0.05F)
      case _ =>
        scoreList.updated(id, 1)
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

  def randomGossip(fanout: Int, sender: Node): List[Long] = 
    Oracle.peerAlgorithm match {
    case 0 => 
      val probability = Random.nextFloat
      scoreList.filter(x => x._1 != 0 && x._1 != sender.getID && x._2 > probability).toList.map(_._1)
    case 1 =>
      scoreList.filter(x => x._1 != 0 && x._1 != sender.getID).toList.map(_._1)
  }
  
  
  def dumpAltruistics =
    scoreList.filter(_._2 > 0.5).map(_._1) toList

  def dumpFreeRiders =
    scoreList.filter(_._2 <= 0).map(_._1) toList

  override def clone(): Object = {
    this.scoreList = Map[Long, Float]()
    this.messageList = Array.fill(BasicGossip.cycles)(None: Option[Info])
    this.waitingConfirm = new MutableList[Int]()
    super.clone()
  }

  private def hasFreeRiders: Boolean = scoreList.values.exists(_ < 0)

  def canAcceptJoinRequest: Boolean =
    Oracle.getLinkable(this).getNeighbors.size < Oracle.fanout + 2 || hasFreeRiders

  def altruisticsNeighbors: List[Long] = scoreList.filter(_._2 > 0.5).map(_._1) toList

//  def addWaitingConfirm(id: Int) =
//    if (!waitingConfirm.contains(id)) waitingConfirm += id
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
