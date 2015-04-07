package basicGossip.protocols

import peersim.core.IdleProtocol
import peersim.core.Node
import basicGossip.oracle.Oracle
import peersim.core.Protocol
import peersim.core.Linkable
import basicGossip.node.Usernode
import peersim.core.Network
import scala.collection.mutable.MutableList

class Link(prefix: String) extends IdleProtocol(prefix) {

  var neigh: MutableList[Usernode] = new MutableList()

  def dumpNeigh = {
    neigh map {
      x =>print(x.getID + " ")
    }
    println
  }

  def getNeighborById(id: Long): Option[Usernode] = {
    neigh.find(_.getID == id)
  }

  def addNeighbor(node: Usernode): Boolean = {
    neigh match {
      case n if !n.contains(node) =>
        neigh = neigh.+:(node)
        true
      case _ => false
    }
  }

  def removeNeighbor(node: Usernode) {
    neigh = neigh.filter(_.getID != node.getID)
  }

  def cleanAll = {
    neigh = MutableList[Usernode]()
  }

  override def onKill = cleanAll

  override def degree = neigh.size

  override def addNeighbor(node: Node): Boolean = addNeighbor(Oracle.getNode(node.getID.toInt))

  override def contains(node: Node): Boolean = neigh.exists(_.getID == node.getID)

  override def getNeighbor(nid: Int): Node = neigh.find(_.getID == nid).getOrElse(Network.get(0))

  //override def clone {}

}