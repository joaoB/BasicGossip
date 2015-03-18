package basicGossip.observers

import basicGossip.node.Usernode
import peersim.core.Network
import basicGossip.protocols.BasicGossip
import basicGossip.oracle.Oracle

class PercentagesObserver(name: String) extends BasicGossipObserver(name) {

  override def run: Boolean = {
    dumpMinAndMaxPercentages
    //dumpFreeRiders
    false
  }

  def dumpFreeRiders = {
    Oracle.allNodesExceptStreamer map (_.dumpFreeRiders)
  }

  def dumpMinAndMaxPercentages = {
    val percentages = Oracle.allNodesExceptStreamer map {
      node => (node.messageList filter (x => x != null)).size.toFloat / BasicGossip.cycles
    }
    println("Min percentage: " + percentages.min)
    println("Max percentage: " + percentages.max)
    println("Amount of nodes with 100% -> " + percentages.count { x => x == 1.0 })
  }

}