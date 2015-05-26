package basicGossip.protocols

import peersim.cdsim.CDProtocol
import peersim.config.Configuration
import peersim.core.Linkable
import peersim.core.Network
import peersim.core.Node
import peersim.edsim.EDProtocol
import peersim.transport.Transport
import peersim.vector.SingleValueHolder
import basicGossip.messages.Info
import basicGossip.node.Usernode
import peersim.config.FastConfig
import scala.util.Random
import utils.DistinctRandom
import basicGossip.oracle.Oracle
import basicGossip.observers.AvgReliability
import hyparview.HyParViewJoinTest

class BasicGossip(prefix: String) extends SingleValueHolder(prefix) with CDProtocol with EDProtocol {

  val networkSize = (Configuration.getInt("network.size", 1))
  val cycles = (Configuration.getInt("CYCLES", 1))
  var info: Int = 0

  //  def a = {
  //        val a = new Thread(new Runnable {
  //      def run {
  //        while (info < cycles) {
  //          /*if (info % 50 == 0) */println("generating new info ///////////////////////////////////////////////////////////////////////////" + info)
  //          sendInfo(Oracle.getNode(0), newInfo, 0)
  //         // Thread.sleep(20)
  //        }
  //        AvgReliability.run
  //      }
  //    })
  //
  //    a.start
  //    if(info + 10 == cycles)a.join
  //  }

  override def nextCycle(node: Node, pid: Int): Unit = {
    //if (info % 50 == 0) println("generating new info " + info)
    //    if (info == 500) {
    //      for (a <- 0 until 1)
    //        Oracle.addAltruisticNode
    //      Oracle.getNode(1000).scoreList
    //      println("NEWWWWWWWWWWWWWWWWWWWWWWWWWW - > " + Oracle.getNode(1000).scoreList)
    //    }

    try {
      println("node: " + 72 + " -> " + Oracle.getNode(72).scoreList.size)
    }
    catch {
      case e: Throwable => 
    }

    if (Network.size < 1000) {
      Oracle.addAltruisticNode
    }

    Oracle.updateCurrentPackage
    sendInfo(Oracle.getNode(0), newInfo, pid)
  }

  private def sendInfo(streamer: Usernode, info: Info, pid: Int) = {
    val link = Oracle.getLinkable(streamer)
    val trans = streamer.getProtocol(FastConfig.getTransport(pid))

    trans match {
      case t: Transport =>
        DistinctRandom.sample(1 until Network.size toList, Math.min(Oracle.fanout, Network.size)) map {
          //Random.shuffle(Oracle.altruistics).take(fanout) map {  
          id =>
            val peern = Oracle.getNode(id)
            if (peern.isUp) {
              t.send(streamer, peern, info, pid)
            }
          //            link.getNeighbor(id) match {
          //              case peern if peern.isUp =>
          //                println("sending " + peern.getID)
          //                t.send(streamer, peern, info, pid)
          //              case peern if !peern.isUp => None
          //            }
        }
    }

    if (link.degree > 0) {

    } else {
      println("Could not send. Is degree > 0 ?")
    }

  }

  private def newInfo: Info = {
    val currentInfo = info;
    info += 1;
    Info(currentInfo, Oracle.getNode(0), 0)
  }

  def processEvent(node: Node, pid: Int, event: Object) {
  }

}

object BasicGossip extends BasicGossip("Basic Gossip")