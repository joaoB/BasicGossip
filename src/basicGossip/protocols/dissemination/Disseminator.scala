package basicGossip.protocols.dissemination

import basicGossip.node.Usernode
import peersim.core.Node
import basicGossip.oracle.Oracle
import scala.util.Random
import basicGossip.messages.Info

abstract class Disseminator {

  def computeFanout(gossiper: Usernode, sender: Node): Set[Long]

  def computeFanout(gossiper: Usernode, sender: Node, info: Info): Set[Long] = {
    Set.empty
  }

  def aboveBaseRank: Boolean = {
    Oracle.forwardProbability > Random.nextFloat
  }

  def belowBaseRank(score: Float): Boolean = {
    val calced = Oracle.forwardProbability * Math.abs((-Oracle.FR_THRESHOLD - (-score)) / Oracle.FR_THRESHOLD)
    calced > Random.nextFloat
  }

}