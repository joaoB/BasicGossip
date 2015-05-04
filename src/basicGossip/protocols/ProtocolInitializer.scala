package basicGossip.protocols

import basicGossip.node.Usernode
import basicGossip.oracle.Oracle
import hyparview.HyParViewJoinTest
import peersim.config.Configuration
import peersim.core.Control
import peersim.core.IdleProtocol
import peersim.core.Network
import peersim.core.Node
import peersim.dynamics.NodeInitializer

class ProtocolInitializer(name: String) extends Control with NodeInitializer {

  val frPercentage = Oracle.frPercentage
  val protocolID = Configuration.getInt(name + "." + "PROTOCOL_ID")
  val streamerID = 0

  def execute = {

    Oracle.altruistics map {
      id => initializeAltruistic(Oracle.getNode(id))
    }

    Oracle.freeRiders map {
      id => initializeFreeRider(Oracle.getNode(id))
    }

    initializeViews
    false
  }

  def initializeAltruistic(node: Usernode) {
    protocolID match {
      case 1 => node.setProtocol(0, new AltruisticProtocol("Altruistic Protocol"))
      case 2 => node.setProtocol(0, new AltruisticWithMaxHops("basicGossip.protocols.AltruisticWithMaxHops"))
      case 3 => node.setProtocol(0, new AltruisticThirdModel("basicGossip.protocols.AltruisticThirdModel"))
      case 4 => node.setProtocol(0, new ThreeFaseGossip("basicGossip.protocols.ThreeFaseGossip"))
      case _ => ???
    }

  }

  def initializeFreeRider(node: Usernode) {
    node.setProtocol(0, new RationalProtocol("Free Rider Protocol"))
  }

  def initialize(node: Node) = {}


  def initializeViews = {
    //hyparview initialization
    Oracle.nodesHpvProtocol map {
      case (un: Usernode, prot: HyParViewJoinTest) => prot.setMyNode(Network.get(un.getID.toInt), Oracle.getViewSize(un))
    }

    val streamerHpv = Oracle.nodeHpvProtocol(streamerID)

    Oracle.allNodesExceptStreamer map {
      node => streamerHpv._2.join(Network.get(node.getID.toInt), HyParViewJoinTest.protocolID)
    }
    rejoinIsolated

    globalHyParViewLinkage
    //traditionalHyParViewLinkage

    /*    Oracle.nodeHpvProtocol(Oracle.getLinkable(2).getNeighbors.head.getID.toInt)._2.simpleJoin(Oracle.getNode(2), HyParViewJoinTest.protocolID)

    Oracle.allNodesExceptStreamer map {
      elem => Oracle.nodeHpvProtocol(Oracle.getLinkable(elem.getID.toInt).getNeighbors.head.getID.toInt)._2.simpleJoin(Oracle.getNode(elem.getID.toInt), HyParViewJoinTest.protocolID)

    }*/

    /*Oracle.nodesHpvProtocol /*filter (_._1.getID == 2) */ map {
      elem =>
        print("NODE: " + elem._1.getID + " -> ")
        elem._2.neighbors map {
          x => print(x.getID + " ")
        }
        println
    }*/
  }

  private def rejoinIsolated = {
    def rejoinIsolatedAux: Boolean = {
      val streamerHpv = Oracle.nodeHpvProtocol(streamerID)
      val a = Oracle.nodesHpvProtocolExceptStreamer.filter(x => x._2.neighbors.size < Oracle.fanout / 2) map {
        a => streamerHpv._2.join(Network.get(a._1.getID.toInt), HyParViewJoinTest.protocolID)
      }
      Oracle.nodesHpvProtocolExceptStreamer.exists(x => x._2.neighbors.size < Oracle.minWindow)
    }
    while (rejoinIsolatedAux) {}
  }

  private def globalHyParViewLinkage = {
    //streamer knows everybody
    val streamer = Oracle.getNode(streamerID)
    val streamerLink = Oracle.getLinkable(streamer)
    Oracle.nodesHpvProtocolExceptStreamer map {
      case (un: Usernode, prot: HyParViewJoinTest) =>
        un.initializeScoreList(prot.neighbors.toSeq map (x => x.getID))
        prot.neighbors map {
          neigh =>
            val unLink = Oracle.getLinkable(un)
            unLink.addNeighbor(Network.get(streamerID))
            unLink.addNeighbor(neigh)
            streamerLink.addNeighbor(un)
        }
    }
  }

  private def traditionalHyParViewLinkage = {
    Oracle.nodesHpvProtocol map {
      case (un: Usernode, prot: HyParViewJoinTest) =>
        un.initializeScoreList(prot.neighbors.toSeq map (x => x.getID))
        prot.neighbors map {
          neigh => Oracle.getLinkable(un).addNeighbor(neigh)
        }
    }
  }

}