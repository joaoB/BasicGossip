package example.basicGossip.protocols

import peersim.core.Node
import scala.util.Random
import peersim.core.Network
import example.basicGossip.Usernode
import peersim.config.FastConfig
import peersim.core.Linkable
import peersim.transport.Transport
import example.basicGossip.Info
import peersim.edsim.EDProtocol
import peersim.cdsim.CDProtocol
import peersim.vector.SingleValueHolder
import peersim.config.Configuration

class BasicGossip(prefix: String) extends SingleValueHolder(prefix) with CDProtocol with EDProtocol {

  val networkSize = (Configuration.getInt("network.size", 1))
  val cycles = (Configuration.getInt("CYCLES", 1))
  val fanout = Math.log(networkSize).toInt
  var info: Int = 0

  override def nextCycle(node: Node, pid: Int): Unit = {
    
    node.getIndex match {
      case 0 => Network.get(0) match {
        case streamer: Usernode => sendInfo(streamer, fanout, pid, newInfo)
      }
      case _ => //only streamer does stuff at start of new cycle
    }
  }

  private def sendInfoAux(streamer: Usernode, info: Info, pid: Int) = {
    streamer.getProtocol(FastConfig.getLinkable(pid)) match {
      case link: Linkable if link.degree > 0 =>
        link.getNeighbor(Random.nextInt(link.degree)) match {
          case peern if peern.isUp => streamer.getProtocol(FastConfig.getTransport(pid)) match {
            case trans: Transport => trans.send(streamer, peern, info, pid)
            case _ => None
          }
          case peern if !peern.isUp => None
        }
      case _ => println("Could not send. Is degree > 0 ?")
    }

  }

  private def sendInfo(streamer: Usernode, fanout: Int, pid: Int, info: Info): Unit = {
    fanout match {
      case 0 =>
      case _ =>
        sendInfoAux(streamer, info, pid)
        sendInfo(streamer, fanout - 1, pid, info)
    }
  }


  private def newInfo: Info = {
    val currentInfo = info;
    info += 1;
    Network.get(0) match {
      case streamer: Usernode => Info(currentInfo, streamer, 0)
    }
  }

  def processEvent(node: Node, pid: Int, event: Object) {
    //for now, no 1 should send messages to streamer
  }

}

object BasicGossip extends BasicGossip("Basic Gossip")

