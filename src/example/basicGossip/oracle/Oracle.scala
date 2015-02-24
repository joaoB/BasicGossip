package example.basicGossip.oracle

import example.basicGossip.Info
import example.basicGossip.Usernode

//Oracle has an eye on everythinggi
class Oracle {

  var maxHopInfo: Info = Info(0, new Usernode("test node"), 0)

  def saveMaxHopInfo(info: Info) {
  //  println("Current Info Hop: " + maxHopInfo.hop)
   // println("Received: " + info.hop)
    if (info.hop > maxHopInfo.hop) {
      updateMaxHopInfo(info)
    }
  }

  private def updateMaxHopInfo(info: Info) {
    maxHopInfo = info
  }

}

object Oracle extends Oracle