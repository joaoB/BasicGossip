package basicGossip.protocols

import peersim.core.IdleProtocol
import peersim.core.Node

class Link(prefix: String) extends IdleProtocol(prefix) {

  def getNeighborById(id: Long): Option[Node] = {
    neighbors.find { x => x.getID == id }
  }

  def removeNeighbor(node: Node) {
    neighbors = neighbors diff List(node)
  }

  def cleanAll = {
    neighbors = new Array[Node](10)

    len = 0
  }

}