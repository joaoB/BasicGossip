package basicGossip.observers

import basicGossip.node.Usernode
import peersim.core.Network
import basicGossip.protocols.BasicGossip

class PercentagesObserver(name: String) extends BasicGossipObserver(name) {
  
    override def run: Boolean = {
    dumpMinAndMaxPercentages
    false
  }
  
  def dumpMinAndMaxPercentages = {
    val percentages = (for (i <- 1 until Network.size) yield Network.get(i)) map {
      elem =>
        elem match {
          case un: Usernode => un.messageList.size.toFloat / BasicGossip.cycles
        }
    }
    println("Min percentage: " + percentages.min)
    println("Max percentage: " + percentages.max)
    println("Amount of nodes with 100% -> " + percentages.count { x => x == 1.0 })
  }

}