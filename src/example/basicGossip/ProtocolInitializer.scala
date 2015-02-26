package example.basicGossip

import peersim.core.Control
import peersim.dynamics.NodeInitializer
import peersim.core.Node
import peersim.core.Network
import peersim.config.Configuration
import example.basicGossip.protocols._

class ProtocolInitializer(name: String) extends Control with NodeInitializer {

  val frPercentage = Configuration.getDouble(name + "." + "FR_PERCENTAGE");

  def execute = {
    for (id <- 1 until Network.size()) {
      id match {
        case id if id < Network.size() * frPercentage => initializeFreeRider(Network.get(id))
        case _ => initializeAltruistic(Network.get(id))
      }
    }
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

  def initialize(node: Node) = {
  }

}