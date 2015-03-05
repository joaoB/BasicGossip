package basicGossip.protocols

import basicGossip.messages.Info
import basicGossip.node.Usernode
import peersim.config.FastConfig
import peersim.core.CommonState
import peersim.core.Linkable
import peersim.transport.Transport
import scala.util.Random
import basicGossip.oracle.Oracle
import peersim.core.Node

class AltruisticProtocol(name: String) extends GeneralProtocol(name) {

//  override def sendMessage(node: Usernode, info: Info, pid: Int) {
//    if (!saveInfo(node, info)) {
//      val linkable = node.getProtocol(FastConfig.getLinkable(pid))
//      for (i <- 0 until BasicGossip.fanout) {
//        linkable match {
//          case link: Linkable =>
//            if (link.degree() > 0) {
//              val peern = link.getNeighbor(Random.nextInt(link.degree))
//              if (peern.isUp()) {
//                node.getProtocol(FastConfig.getTransport(pid)) match {
//                  case trans: Transport =>
//                    sendInfo(trans, node, peern, Info(info.value, node, info.hop + 1), pid)
//                  case _ => ???
//                }
//              }
//            }
//        }
//      }
//    }
//  }

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
                      case trans: Transport =>
                        sendInfo(trans, node, peern, Info(info.value, node, info.hop + 1), pid)
                      case _ => ???
                    }
                  case _ => sendToRandom(node, Info(info.value, node, info.hop + 1), pid)
                }

              }
          }
      }
    }
  } 


}