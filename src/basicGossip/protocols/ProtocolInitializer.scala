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
    for (i <- 0 until Network.size) {
      Network.get(i).getProtocol(HyParViewJoinTest.protocolID) match {
        case prot: HyParViewJoinTest => prot.setMyNode(Network.get(i))
        case _ => println("Check initializeViews@protocol initializer")
      }
    }
    //hyparview initialization
    Network.get(0).getProtocol(HyParViewJoinTest.protocolID) match {
      case prot: HyParViewJoinTest => for (i <- 1 until Network.size) { prot.join(Network.get(i), HyParViewJoinTest.protocolID) }
      case _ => println("Check initializeViews@protocol initializer")
    }

    globalHyParViewLinkage
    // traditionalHyParViewLinkage
  }

  private def globalHyParViewLinkage = {
    //streamer knows everybody
    Network.get(0) match {
      case streamer: Usernode =>
        getLinkable(streamer) match {
          case streamerLink: Link =>
            for (i <- 1 until Network.size) {
              val node = Network.get(i) match {
                case un: Usernode =>
                  un.getProtocol(HyParViewJoinTest.protocolID) match {
                    case prot: HyParViewJoinTest =>
                      un.initializeScoreList(prot.neighbors.toSeq map (x => x.getID))
                      prot.neighbors map {
                        neigh =>
                          getLinkable(un) match {
                            case protLink: Link =>
                              protLink.addNeighbor(Network.get(0))
                              protLink.addNeighbor(neigh)
                              streamerLink.addNeighbor(un)
                            case _ => println("Check initializeViews@protocol initializer")
                          }
                      }
                  }
              }
            }
        }
    }

  }

  private def traditionalHyParViewLinkage = {
    //put result from hyparview into link protocol
    for (i <- 0 until Network.size) {
      Network.get(i) match {
        case un: Usernode =>
          un.getProtocol(HyParViewJoinTest.protocolID) match {
            case prot: HyParViewJoinTest =>
              un.initializeScoreList(prot.neighbors.toSeq map (x => x.getID))
              prot.neighbors map {
                neigh =>
                  getLinkable(un) match {
                    case protLink: Link => protLink.addNeighbor(neigh)
                    case _ => println("Check initializeViews@protocol initializer")
                  }
              }
          }
      }
    }
  }

  private def getLinkable(usernode: Usernode) = usernode.getProtocol(FastConfig.getLinkable(0))
  
}