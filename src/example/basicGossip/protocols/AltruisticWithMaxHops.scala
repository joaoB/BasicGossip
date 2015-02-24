package example.basicGossip.protocols

import example.basicGossip.Info
import peersim.transport.Transport
import example.basicGossip.Usernode
import peersim.core.Linkable
import peersim.config.FastConfig
import scala.util.Random
import peersim.config.Configuration

class AltruisticWithMaxHops(name: String) extends AltruisticProtocol(name) {

  val maxHops = Configuration.getInt(name + "." + "MAX_HOPS")

  override def sendMessage(node: Usernode, info: Info, pid: Int) {
    saveInfo(node, info)
    val linkable = node.getProtocol(FastConfig.getLinkable(pid))
    for (i <- 0 until BasicGossip.fanout) {
      linkable match {
        case link: Linkable =>
          if (link.degree() > 0) {
            val peern = link.getNeighbor(Random
              .nextInt(link.degree));
            if (peern.isUp()) {
              node.getProtocol(FastConfig.getTransport(pid)) match {
                case trans: Transport if info.hop < maxHops =>
                  trans.send(node, peern, Info(info.value, node, info.hop + 1), pid)
                  node.decreaseScore(peern)
                case _ => // do nothing
              }
            }
          }
      }
    }

  }
}