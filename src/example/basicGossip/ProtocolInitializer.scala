package example.basicGossip

import peersim.core.Control
import peersim.dynamics.NodeInitializer
import peersim.core.Node
import peersim.core.Network

class ProtocolInitializer(name: String) extends Control with NodeInitializer{

  def execute = {
    for(id <- 1 until Network.size()) {
      println("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
     initialize(Network.get(id))
      
    }
    false
  }
  
  def initialize(node: Node) = {
    node match {
      case myNode : Usernode => myNode.setProtocol(0, new AltruisticProtocol("a"))
      case _ => ???
    }
  } 
  
}