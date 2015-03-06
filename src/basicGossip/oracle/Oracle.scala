package basicGossip.oracle

import basicGossip.messages.Info
import basicGossip.node.Usernode
import scala.collection.mutable.MutableList
import peersim.config.Configuration

//Oracle has an eye on everythinggi
class Oracle {

  val frPercentage = Configuration.getDouble("Oracle." + "FR_PERCENTAGE")

  var maxHopInfo: Option[Info] = None
  var amountOfSentMessages: Int = 0
  var avgHops = MutableList[Int]()

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

  def saveHop(info: Info) = {
    avgHops.+=(info.hop)
  }

}

object Oracle extends Oracle