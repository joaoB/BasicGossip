package basicGossip.protocols.dissemination

import basicGossip.node.Usernode
import peersim.core.Node
import basicGossip.node.NodeStatus
import basicGossip.oracle.Oracle

class Flooding extends Disseminator {
  override def computeFanout(gossiper: Usernode, sender: Node): Set[Long] = {
    (gossiper.scoreList.filter(x => x._1 != 0 && x._1 != sender.getID && x._2.status == NodeStatus.ACTIVE)).keySet
  }

}

object FloodingDisseminator extends Flooding