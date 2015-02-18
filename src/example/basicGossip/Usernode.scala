package example.basicGossip

import peersim.core.GeneralNode
import scala.collection.mutable.MutableList
import peersim.core.ModifiableNode

class Usernode(prefix: String) extends ModifiableNode(prefix){
  
 
  val messageList = MutableList[Int]()
  
  def saveMessage(elem: Int) = {
    messageList.+=(elem)
  }
  
  def containsElem(elem: Int): Boolean = {
    messageList.contains(elem)
  }
  
  def dumpMessageList = {
    print("Node: " + this.getID + "->")
    messageList.map(x => print(x + " "))
    println()
  }


  
}
