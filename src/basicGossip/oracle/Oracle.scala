package basicGossip.oracle

import basicGossip.messages.Info
import basicGossip.node.Usernode

//Oracle has an eye on everythinggi
class Oracle {

  var maxHopInfo: Option[Info] = None
  var amountOfSentMessages: Int = 0
  
  def saveMaxHopInfo(info: Info) {
    //  println("Current Info Hop: " + maxHopInfo.hop)
    // println("Received: " + info.hop)
    maxHopInfo match {
      case Some(elem) if info.hop > elem.hop => updateMaxHopInfo(info)
      case None => updateMaxHopInfo(info)
      case _ =>
    }
  }
  
  def incSentMessages = amountOfSentMessages += 1

  private def updateMaxHopInfo(info: Info) {
    maxHopInfo = Some(info)
  }

}

object Oracle extends Oracle