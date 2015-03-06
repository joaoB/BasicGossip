package basicGossip.observers

import basicGossip.node.Usernode
import basicGossip.protocols.BasicGossip
import peersim.core.Network

class AvgReliability(name: String) extends BasicGossipObserver(name) {
  
  override def run: Boolean = {
    dumpAvgReliability 
    false
  }
  
  def dumpAvgReliability = {
    val percentages = (for (i <- 1 until Network.size) yield Network.get(i)) map {
      elem =>
        elem match {
          case un: Usernode => un.messageList.size.toFloat / BasicGossip.cycles
        }
    }
    println("Reliability: " + percentages.sum / percentages.size)
  }

}