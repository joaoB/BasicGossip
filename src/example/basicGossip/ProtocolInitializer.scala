package example.basicGossip

import peersim.core.Control
import peersim.dynamics.NodeInitializer
import peersim.core.Node
import peersim.core.Network
import peersim.config.Configuration
import example.basicGossip.protocols._
import hyparview.HyParViewJoinTest
import peersim.core.Linkable
import peersim.core.IdleProtocol
import peersim.config.FastConfig

class ProtocolInitializer(name: String) extends Control with NodeInitializer {

  val frPercentage = Configuration.getDouble(name + "." + "FR_PERCENTAGE");

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
      case myNode: Usernode => myNode.setProtocol(0, new AltruisticProtocol("Altruistic Protocol"))
      //case myNode: Usernode => myNode.setProtocol(0, new AltruisticWithMaxHops("example.basicGossip.protocols.AltruisticWithMaxHops"))
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

    for (i <- 0 until Network.size) {
      Network.get(i).getProtocol(HyParViewJoinTest.protocolID) match {
        case prot: HyParViewJoinTest => prot.setMyNode(Network.get(i))
        case _ => println("Check initializeViews@protocol initializer")
      }
    }
    for (i <- 0 until Network.size) {
      Network.get(0).getProtocol(HyParViewJoinTest.protocolID) match {
        case prot: HyParViewJoinTest => prot.join(Network.get(i), HyParViewJoinTest.protocolID)
        case _ => println("Check initializeViews@protocol initializer")
      }
    }

    for (i <- 0 until Network.size) {
      Network.get(i).getProtocol(FastConfig.getLinkable(0)) match {
        case prot: Link => prot.cleanAll
        case _ => println("Check initializeViews@protocol initializer")
      }
    }

    for (i <- 0 until Network.size) {
      Network.get(i).getProtocol(FastConfig.getLinkable(0)) match {
        case prot: Link => prot.cleanAll
        case _ => println("Check initializeViews@protocol initializer")
      }
    }

    for (i <- 0 until Network.size) {
      val node = Network.get(i)
      node.getProtocol(HyParViewJoinTest.protocolID) match {
        case prot: HyParViewJoinTest => prot.neighbors map {
          neigh =>
            node.getProtocol(FastConfig.getLinkable(0)) match {
              case protLink: Link => protLink.addNeighbor(neigh)
              case _ => println("Check initializeViews@protocol initializer")
            }
        }
      }
    }

  }

}