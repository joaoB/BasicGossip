package basicGossip.protocols

import basicGossip.messages.Info
import basicGossip.node.Usernode
import peersim.config.Configuration
import peersim.transport.Transport
import peersim.config.FastConfig

class AltruisticThirdModel(name: String) extends AltruisticProtocol(name) {

  val maxHops = Configuration.getInt(name + "." + "MAX_HOPS")

  override def sendMessage(node: Usernode, info: Info, pid: Int) {
    if (!saveInfo(node, info)) {
      val linkable = node.getProtocol(FastConfig.getLinkable(pid))
      node.randomGossip(BasicGossip.fanout, info.sender) map {
        id =>
          linkable match {
            case link: Link =>
              if (link.degree() > 0) {
                val peern = link.getNeighborById(id) match {
                  case Some(peern) if peern.isUp =>
                    node.getProtocol(FastConfig.getTransport(pid)) match {
                      case trans: Transport if info.hop < maxHops =>
                        sendInfo(trans, node, peern, Info(info.value, node, info.hop + 1), pid)
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