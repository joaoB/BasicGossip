package basicGossip.protocols

import basicGossip.node.Usernode

class WaitConnectionsAttack(name: String) extends AltruisticProtocol(name) {
  
  override def canAcceptNewNeighbor(un: Usernode) = {
    println("you can connect babe")
    true
  }

}