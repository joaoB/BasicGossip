package example.basicGossip

import scala.collection.mutable.MutableList
import scala.util.Random

import example.basicGossip.protocols.BasicGossip
import peersim.config.FastConfig
import peersim.core.Linkable
import peersim.core.ModifiableNode
import peersim.core.Node

class Usernode(prefix: String) extends ModifiableNode(prefix) {

  var messageList = MutableList[Int]()
  var scoreList = Map[Long, Int]()

  def saveMessage(elem: Int) = {
    messageList.+=(elem)
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

  def randomGossip(fanout: Int, sender: Node): List[Long] = {

    val calculated = scoreList.filter {
      x => x._1 != 0 || x._1 != sender.getID
    }
    //.toSeq.sortBy(x => -x._2)
    //.take(fanout)
    Random.shuffle(calculated).take(fanout).map(x => x._1).toList

  }

  def containsElem(elem: Int): Boolean = {
    messageList.contains(elem)
  }

  def dumpMessageList = {
    print("Node: " + this.getID + "->")
    messageList.map(x => print(x + " "))
    println()
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

  def dumpPercentageOfMessage = {
    print("Node: " + this.getID + "-> ")
    println(messageList.size.toFloat / BasicGossip.cycles)
  }

  def dumpAmoutOfMessage = {
    print("Node: " + this.getID + "-> ")
    println(messageList.size + " / " + BasicGossip.cycles)
  }

  def dumpFreeRiders = {
    print("Free Riders of node: " + getID + " -> ")
    val fr = scoreList.filter {
      x => x._2 <= -3
    }
      .map {
        elems => print(elems._1 + " ")
      }
    println()
  }

  override def clone(): Object = {
    this.scoreList = Map[Long, Int]()
    this.messageList = new MutableList[Int]()
    super.clone()
  }

}
