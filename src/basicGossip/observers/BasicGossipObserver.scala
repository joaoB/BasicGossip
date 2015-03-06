package basicGossip.observers

import basicGossip.oracle.Oracle
import basicGossip.protocols.BasicGossip
import peersim.core.Control
import peersim.core.Network
import basicGossip.node.Usernode

abstract class BasicGossipObserver(name: String) extends Control {

  def execute = {
    run
  }

  def run: Boolean

}