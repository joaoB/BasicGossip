package basicGossip.observers

import basicGossip.node.Usernode
import basicGossip.protocols.BasicGossip
import peersim.core.Network
import peersim.config.FastConfig
import peersim.core.Linkable
import hyparview.HyParViewJoinTest
import basicGossip.protocols.Link
import basicGossip.oracle.Oracle

class AvgReliability(name: String) extends BasicGossipObserver(name) {

  override def run: Boolean = {
    dumpAvgReliability
    reliabilityAbovePercentage
    dumpNodesWithZeroMessages
    false
  }

  def dumpAvgReliability = {
    val percentages = Oracle.allNodesExceptStreamer map (_.messageList.size.toFloat / BasicGossip.cycles)
    println("Reliability1: " + percentages.sum / percentages.size)
  }

  def reliabilityAbovePercentage = {
    val percentages = Oracle.allNodesExceptStreamer map (_.messageList.size.toFloat / BasicGossip.cycles)
    val aboveScore = percentages filter (_ > 0.98)
    println("Reliability2: " + aboveScore.size.toFloat / Network.size)

  }

  def dumpNodesWithZeroMessages = {
    Oracle.nodesHpvProtocol(
      Oracle.allNodesExceptStreamer filter (_.messageList.size == 0)) map {
        elem =>
          print("Node: " + elem._1 + " -> ")
          elem._2.neighbors.map(x => print(x.getID + " "))
          println
      }
  }

}