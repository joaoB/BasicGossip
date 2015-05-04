package basicGossip.observers

import basicGossip.oracle.Oracle
import basicGossip.protocols.ProtocolInitializer
import peersim.core.Network

class TotalMessagesObserver(name: String) extends BasicGossipObserver(name) {

  override def run: Boolean = {
    dumpTotalMessages
    avgAltruisticMessage
    false
  }

  def dumpTotalMessages = {
    println("Amounf of messages: " + Oracle.altruisticsAmountOfSentMessages + Oracle.frAmountOfSentMessages)
  }
  
  def avgAltruisticMessage = {
    val altruistics = (1 - Oracle.frPercentage) * Network.size
    println("Avg Altruistic Message #: " + Oracle.altruisticsAmountOfSentMessages / altruistics )
    
    println("Avg FR Message #: " + Oracle.frAmountOfSentMessages / Oracle.frAmount )
    
  }
}