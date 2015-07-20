package basicGossip.protocols.dissemination

import basicGossip.node.Usernode
import peersim.core.Node

class FreeRider extends Disseminator {
  override def computeFanout(gossiper: Usernode, sender: Node): Set[Long] = Set[Long]().empty

}

object FRDisseminator extends FreeRider