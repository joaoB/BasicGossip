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
import basicGossip.protocols.GeneralProtocol.Heavyweight

class BasicGossip(prefix: String) extends SingleValueHolder(prefix) with CDProtocol with EDProtocol {

  val networkSize = (Configuration.getInt("network.size", 1))
  val cycles = (Configuration.getInt("CYCLES", 1))
  var info: Int = 0

  override def nextCycle(node: Node, pid: Int): Unit = {

    if (Network.size < 100) {
      Oracle.addAltruisticNode
    } else if (Oracle.currentPackage == 100) {
      Oracle.allNodesExceptStreamer map {
        node =>
          node.behaviorProtocol match {
            case h: Heavyweight => h.setManagers(node)
            case _ =>
          }
      }
    }

    if (Oracle.currentPackage == 999) {
      Oracle.injectFreeRiders
    }

    for (id <- 1 until Network.size) {
      val node = Oracle.getNode(id)
      node.getProtocol(3) match {
        case p: CDProtocol =>
          p.nextCycle(node, 3)
      }
      node.getProtocol(2) match {
        case p: CDProtocol =>
          p.nextCycle(node, 2)
      }
      //node.requestProposals

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
              //println("sendin to " + peern.getID)
              t.send(streamer, peern, info, pid)
            }
        }
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