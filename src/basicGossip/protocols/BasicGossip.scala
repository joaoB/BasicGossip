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

class BasicGossip(prefix: String) extends SingleValueHolder(prefix) with CDProtocol with EDProtocol {

  val networkSize = (Configuration.getInt("network.size", 1))
  val cycles = (Configuration.getInt("CYCLES", 1))
  val fanout = Math.log(networkSize).toInt
  var info: Int = 0

  override def nextCycle(node: Node, pid: Int): Unit = {
    //println("generating new info " + info)
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
        sample(0 until link.degree toList, fanout) map {
          id =>
            link.getNeighbor(id) match {
              case peern if peern.isUp => streamer.getProtocol(FastConfig.getTransport(pid)) match {
                case trans: Transport =>
                  //println("streamer ending to " + id)
                  trans.send(streamer, peern, info, pid)
                case _ => None
              }
              case peern if !peern.isUp => None
            }
        }
      case _ => println("Could not send. Is degree > 0 ?")
    }

  }

  private def sendInfo(streamer: Usernode, fanout: Int, pid: Int, info: Info): Unit = {
    fanout match {
      case 0 =>
      case _ =>
        sendInfoAux(streamer, info, pid)
       // sendInfo(streamer, fanout - 1, pid, info)
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
    //  println("STREAMER GOT A MESSAGE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  }

  private def sample[A](itms: List[A], sampleSize: Int) = {
    def collect(vect: Vector[A], sampleSize: Int, acc: List[A]): List[A] = {
      if (sampleSize == 0) acc
      else {
        val index = Random.nextInt(vect.size)
        collect(vect.updated(index, vect(0)) tail, sampleSize - 1, vect(index) :: acc)
      }
    }

    collect(itms toVector, sampleSize, Nil)
  } //> sample: [A](itms: List[A], sampleSize: Int)List[A] 

}

object BasicGossip extends BasicGossip("Basic Gossip")

