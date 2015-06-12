package basicGossip.oracle

import basicGossip.node.Usernode
import peersim.core.Network
import basicGossip.protocols.ProtocolName

protected abstract class AllNodes {

  def total = 1 until Network.size toList

  var altruistics = allNodesExceptStreamer.filter(_.behaviorProtocol.protocolName == ProtocolName.ALT).map(_.getID.toInt)
  var freeRiders = allNodesExceptStreamer.filter(_.behaviorProtocol.protocolName == ProtocolName.FR).map(_.getID.toInt)

  def frAmount = freeRiders.size
  
  private def allNodesAux(start: Int): List[Usernode] =
    (for (i <- start until Network.size) yield Network.get(i)).map {
      case un: Usernode => un
    } toList

  def allNodesExceptStreamer = allNodesAux(1)

  def allNodes = allNodesAux(0)

}