package basicGossip.protocols.dissemination

import basicGossip.node.Usernode
import peersim.core.Node
import basicGossip.node.NodeStatus
import basicGossip.oracle.Oracle
import utils.DistinctRandom
import peersim.core.Network
import basicGossip.messages.Info

class Lifting extends Disseminator {

  override def computeFanout(gossiper: Usernode, sender: Node): Set[Long] = {
    val idSet = (1L until Network.size).toList diff (List(gossiper.getID, sender.getID) ++ gossiper.blackList)
    DistinctRandom.sample(idSet, Oracle.fanout).toSet
  }

  override def computeFanout(gossiper: Usernode, sender: Node, info: Info): Set[Long] = {
    computeFanout(gossiper, sender)
  }

}

object LiftingDisseminator extends Lifting