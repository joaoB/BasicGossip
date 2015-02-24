package example.basicGossip.protocols

import example.basicGossip.Info
import example.basicGossip.Usernode

import peersim.config.FastConfig
import peersim.core.CommonState
import peersim.core.Linkable
import peersim.transport.Transport

class AltruisticProtocol(name: String) extends GeneralProtocol(name) {

  override def sendMessage(node: Usernode, info: Info, pid: Int) {
    //TODO: decrease score of the node to whom i will send info
    if (!saveInfo(node, info)) {
      val linkable = node.getProtocol(FastConfig.getLinkable(pid))
      for (i <- 0 until BasicGossip.fanout) {
        linkable match {
          case link: Linkable =>
            if (link.degree() > 0) {
              val peern = link.getNeighbor(CommonState.r
                .nextInt(link.degree));
              if (peern.isUp()) {
                node.getProtocol(FastConfig.getTransport(pid)) match {
                  case trans: Transport =>
                    trans.send(node, peern, new Info(info.value, node), pid)
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