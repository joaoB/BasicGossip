package example.basicGossip.protocols

import example.basicGossip.Info
import example.basicGossip.Usernode
import peersim.config.FastConfig
import peersim.core.CommonState
import peersim.core.Linkable
import peersim.transport.Transport
import scala.util.Random

class AltruisticProtocol(name: String) extends GeneralProtocol(name) {

  override def sendMessage(node: Usernode, info: Info, pid: Int) {
    if (!saveInfo(node, info)) {
      val linkable = node.getProtocol(FastConfig.getLinkable(pid))
      for (i <- 0 until BasicGossip.fanout) {
        linkable match {
          case link: Linkable =>
            if (link.degree() > 0) {
              val peern = link.getNeighbor(Random
                .nextInt(link.degree));
              if (peern.isUp()) {
                node.getProtocol(FastConfig.getTransport(pid)) match {
                  case trans: Transport =>
                    trans.send(node, peern, Info(info.value, node, info.hop + 1), pid)
                    node.decreaseScore(peern)
                  case _ => ???
                }
              }
            }
        }
      }

    }
  }

}