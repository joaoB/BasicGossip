package example.basicGossip

import example.basicGossip.protocols.BasicGossip;
import peersim.core.GeneralNode
import scala.collection.mutable.MutableList
import peersim.core.ModifiableNode

class Usernode(prefix: String) extends ModifiableNode(prefix) {

  var messageList = MutableList[Int]()
  var scoreList = Map[Long, Int]()

  def saveMessage(elem: Int) = {
    messageList.+=(elem)
  }

  def increaseScore(node: Usernode) {
    // used when we receive info from node
    val id = node.getID
    scoreList match {
      case map if map.contains(id) => scoreList.updated(id, scoreList(id) + 1)
      case _ => scoreList += (id -> 1)
    }
  }

  def decreaseScore(node: Usernode) {
    // used when we send info to node
    val id = node.getID
    scoreList match {
      case map if map.contains(id) => scoreList.updated(id, scoreList(id) - 1)
      case _ => scoreList += (id -> -1)
    }
  }

  def containsElem(elem: Int): Boolean = {
    messageList.contains(elem)
  }

  def dumpMessageList = {
    print("Node: " + this.getID + "->")
    messageList.map(x => print(x + " "))
    println()
  }

  def dumpPercentageOfMessage = {
    print("Node: " + this.getID + "-> ")
    println(messageList.size.toFloat / (BasicGossip.cycles - 1))
  }

  override def clone(): Object = {
    this.scoreList = Map[Long, Int]()
    this.messageList = new MutableList[Int]()
    super.clone()
  }

}
