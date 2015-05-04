package basicGossip.protocols

import basicGossip.messages.Info
import basicGossip.node.Usernode
import peersim.config.Configuration
import peersim.transport.Transport
import peersim.config.FastConfig
import basicGossip.oracle.Oracle

class AltruisticThirdModel(name: String) extends AltruisticProtocol(name) {

  override def gossipMessage(node: Usernode, info: Info, pid: Int) {
    if (saveInfo(node, info)) {
      val linkable = node.getProtocol(FastConfig.getLinkable(pid))
      gossipMessage(node, info.sender) map {
        id =>
          linkable match {
            case link: Link =>
              if (link.degree() > 0) {
                val peern = link.getNeighborById(id) match {
                  case Some(peern) if peern.isUp =>
                    node.getProtocol(FastConfig.getTransport(pid)) match {
                      case trans: Transport if info.hop < maxHops =>
                        sendInfo(node, peern, Info(info.value, node, info.hop + 1), pid)
                      case _ => //do nothing
                    }
                  case _ => sendToRandom(node, Info(info.value, node, info.hop + 1), pid)
                }

              }
          }
      }

    }
  }

}