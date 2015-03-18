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

  var messageList: Array[Info]= new Array[Info](BasicGossip.cycles)
  var scoreList = Map[Long, Int]()

  def saveMessage(info: Info) = {
    messageList(info.value) = info
  }

  def containsElem(value: Int): Boolean = {
    messageList(value) != null
  }

  def increaseScore(node: Node) {
    // used when we receive info from node
    val id = node.getID
    scoreList = scoreList match {
      case map if map.contains(id) =>
        scoreList.updated(id, scoreList(id) + 1)
      case _ =>
        scoreList.updated(id, 1)
    }
  }

  def decreaseScore(node: Node) {
    // used when we send info to node
    val id = node.getID
    scoreList = scoreList match {
      case map if map.contains(id) =>
        scoreList.updated(id, scoreList(id) - 1)
      case _ =>
        scoreList.updated(id, -1)
    }
  }

  def initializeScoreList(ids: Seq[Long]) = {
    scoreList = Map(ids map {
      id => id -> 0
    }: _*)
  }

  def getHpvProtocol: HyParViewJoinTest =
    this.getProtocol(HyParViewJoinTest.protocolID) match {
      case prot: HyParViewJoinTest => prot
    }

  def randomGossip(fanout: Int, sender: Node): List[Long] = {
    Oracle.peerAlgorithm match {
      case 0 =>
        val calculated = Random.shuffle(scoreList.filter { x => x._1 != 0 && x._1 != sender.getID && x._2 > -15 })
        val goodNodes = calculated.filter(_._2 > 7).toList.map(_._1)
        goodNodes ++
          calculated.take(fanout - goodNodes.size).toList.map(_._1)
      case 1 =>
        val calculated = Random.shuffle(scoreList.filter { x => x._1 != 0 && x._1 != sender.getID })
        calculated.take(fanout).map(_._1) toList
    }
  }

  def dumpAltruistics = {
    print("Altruistics of node: " + getID + " -> ")
    //val altruistic =
    scoreList.filter {
      x => x._2 >= 0
    }
      .map {
        elems => print(elems._1 + " ")
      }
    println()
  }

  def dumpFreeRiders = {
    val neigh = this.getProtocol(HyParViewJoinTest.protocolID) match {
      case prot: HyParViewJoinTest => Some(prot.neighbors)
      case _ => None
    }
    print("Free Riders of node: " + getID + " -> ")
    val fr = scoreList.filter {
      x => x._2 <= -15
    }
    fr.map {
      elems => print(elems._1 + " ")
    }
    println()

    val FRneib = neigh match {
      case Some(elem) => elem.count { x => x.getID < Oracle.frPercentage * Network.size }
      case None => 0
    }
    println("Found " + fr.size + "/" + FRneib)
    //  println("Found " + neigh)
  }

  override def clone(): Object = {
    this.scoreList = Map[Long, Int]()
    this.messageList = new Array[Info](BasicGossip.cycles)
    super.clone()
  }

}
