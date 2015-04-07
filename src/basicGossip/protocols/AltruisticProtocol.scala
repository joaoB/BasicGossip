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

class AltruisticProtocol(name: String) extends GeneralProtocol {

  override def gossipMessage(node: Usernode, info: Info, pid: Int) {    
    if (!saveInfo(node, info)) {
      val linkable = Oracle.getLinkable(node)
      SearchNewNeighbor.execute(node)
      node.randomGossip(Oracle.fanout, info.sender) map {
        id =>
          linkable.getNeighborById(id) match {
            case Some(peern) if peern.isUp =>
              {
                node.getProtocol(FastConfig.getTransport(pid)) match {
                  case trans: Transport =>
                    //if (node.getID == 1) print(node.scoreList)
                    sendInfo(trans, node, peern, Info(info.value, node, info.hop + 1), pid)
                  case _ => ???
                }
              }

            case _ =>
              print("node -> " + node.getID + " id: " + id + " -- link -- " + "")
              linkable.dumpNeigh //sendToRandom(node, Info(info.value, node, info.hop + 1), pid)
          }

      }
    }
    //    if (node.getID == 1) println

  }
}