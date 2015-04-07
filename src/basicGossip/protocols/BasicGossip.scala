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
  val fanout = Math.log(networkSize).toInt
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

    if (info == 550) {
      Oracle.addAltruisticNode
    }

    sendInfo(Oracle.getNode(0), newInfo, pid)
  }

  private def sendInfo(streamer: Usernode, info: Info, pid: Int) = {
    val link = Oracle.getLinkable(streamer)
    // println("sending")
    if (link.degree > 0) {
      DistinctRandom.sample(0 until link.degree toList, fanout) map {
        id =>
          link.getNeighbor(id) match {
            case peern if peern.isUp =>
              streamer.getProtocol(FastConfig.getTransport(pid)) match {
                case trans: Transport =>
                  //println("sending to " + peern.getID)
//                  if (info.value == 520) {
//                    trans.send(streamer, Oracle.getNode(1000), info, pid)
//                  }
                  trans.send(streamer, peern, info, pid)
                case _ => None
              }
            case peern if !peern.isUp => None
          }
      }
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
    //for now, no 1 should send messages to streamer
  }

}

object BasicGossip extends BasicGossip("Basic Gossip")