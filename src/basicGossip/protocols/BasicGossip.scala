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

class BasicGossip(prefix: String) extends SingleValueHolder(prefix) with CDProtocol with EDProtocol {

  val networkSize = (Configuration.getInt("network.size", 1))
  val cycles = (Configuration.getInt("CYCLES", 1))
  val fanout = Math.log(networkSize).toInt
  var info: Int = 0

  override def nextCycle(node: Node, pid: Int): Unit = {
    if (info % 50 == 0) println("generating new info " + info)

    node.getIndex match {
      case 0 => sendInfo(Oracle.getNode(0), newInfo, pid)
      case _ => //only streamer does stuff at start of new cycle
    }
  }

  private def sendInfo(streamer: Usernode, info: Info, pid: Int) = {
    val link = Oracle.getLinkable(streamer)
    if (link.degree > 0) {
      DistinctRandom.sample(0 until link.degree toList, fanout) map {
        id =>
          link.getNeighbor(id) match {
            case peern if peern.isUp =>
              streamer.getProtocol(FastConfig.getTransport(pid)) match {
                case trans: Transport =>
                  //println("streamer ending to " + id)
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

