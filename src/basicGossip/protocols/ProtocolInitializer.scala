package basicGossip.protocols

import peersim.core.Control
import peersim.dynamics.NodeInitializer
import peersim.core.Node
import peersim.core.Network
import peersim.config.Configuration
import hyparview.HyParViewJoinTest
import peersim.core.Linkable
import peersim.core.IdleProtocol
import peersim.config.FastConfig
import basicGossip.node.Usernode
import basicGossip.oracle.Oracle

class ProtocolInitializer(name: String) extends Control with NodeInitializer {

  val frPercentage = Oracle.frPercentage
  val protocolID = Configuration.getInt(name + "." + "PROTOCOL_ID")
  val streamerID = 0

  def execute = {
    for (id <- 1 until Network.size()) {
      id match {
        case id if id < Network.size() * frPercentage => initializeFreeRider(Network.get(id))
        case _ => initializeAltruistic(Network.get(id))
      }
    }
    initializeViews
    false
  }

  def initializeAltruistic(node: Node) {
    node match {
      case myNode: Usernode =>
        protocolID match {
          case 1 => myNode.setProtocol(0, new AltruisticProtocol("Altruistic Protocol"))
          case 2 => myNode.setProtocol(0, new AltruisticWithMaxHops("basicGossip.protocols.AltruisticWithMaxHops"))
          case 3 => myNode.setProtocol(0, new AltruisticThirdModel("basicGossip.protocols.AltruisticThirdModel"))
          case 4 => myNode.setProtocol(0, new ThreeFaseGossip("basicGossip.protocols.ThreeFaseGossip"))
          case _ => ???
        }
      case _ => ???
    }
  }

  def initializeFreeRider(node: Node) {
    node match {
      case myNode: Usernode => myNode.setProtocol(0, new FRProtocol("Free Rider Protocol"))
      case _ => ???
    }
  }

  def initialize(node: Node) = {}

  def initializeViews = {
    //hyparview initialization
    Oracle.nodesHpvProtocol map {
      case (un: Usernode, prot: HyParViewJoinTest) => prot.setMyNode(Network.get(un.getID.toInt))
    }

    val streamerHpv = Oracle.nodeHpvProtocol(streamerID)
    Oracle.allNodesExceptStreamer map {
      node => streamerHpv._2.join(Network.get(node.getID.toInt), HyParViewJoinTest.protocolID)
    }

    globalHyParViewLinkage
    //traditionalHyParViewLinkage
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