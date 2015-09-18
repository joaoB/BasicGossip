package basicGossip.oracle

import scala.util.Random

import basicGossip.node.Usernode
import basicGossip.protocols.AltruisticProtocol
import basicGossip.protocols.FRProtocol
import basicGossip.protocols.GeneralProtocol.GeneralProtocol
import basicGossip.protocols.GeneralProtocol.H
import basicGossip.protocols.GeneralProtocol.Heavyweight
import basicGossip.protocols.GeneralProtocol.ProtocolName
import basicGossip.protocols.Lifting.Lifting
import basicGossip.protocols.Lifting.LiftingFreeRider
import basicGossip.protocols.Lifting.LiftingRational
import basicGossip.protocols.RationalProtocol
import basicGossip.protocols.dissemination.FreeRider
import hyparview.MyHyParView
import peersim.config.Configuration
import peersim.core.Network

protected abstract class AddNode extends AllNodes {

  val frPercentage = Configuration.getDouble("Oracle.FR_PERCENTAGE")

  private def addNodeAux(protocol: GeneralProtocol): Usernode = {
    val n = new Usernode("Usernode")
    Network.add(n)
    val node = Oracle.getNode(n.getID.toInt)
    node.setProtocol(0, protocol)

    val nodeID = node.getID.toInt
    val nodeNode = Network.get(nodeID)

    for (id <- 0 until protocol.baseWin) {
      val lst = (1 until Network.size toList).diff(List(nodeID)).diff(node.scoreList.keySet toList)
      lst match {
        case Nil => MyHyParView.join(node)
        case x => MyHyParView.join(node, Oracle.getNode(Random.shuffle(x).head))
      }
    }
    node

  }

  def addAltruisticNode = {
    val node = addNodeAux(new AltruisticProtocol("Altruistic"))
    altruistics = allNodesExceptStreamer.filter(_.behaviorProtocol.protocolName == ProtocolName.ALT).map(_.getID.toInt)
  }

  def addFreeRider = {
    val node = addNodeAux(new FRProtocol("Free Rider"))
  }

  def addRational = {
    val node = addNodeAux(new RationalProtocol("RATIONAL"))
  }

  def injectFreeRiders = {
    val amount = 300
    val fr = Random.shuffle((3 until Network.size).toList).take(amount)
    allNodesExceptStreamer filter (node => fr.contains(node.getID)) map {
      x =>
        x.setProtocol(0, new FRProtocol("free rider injected"))
        //H.setManagers(x)

      x.dropConnections
    }
    altruistics = allNodesExceptStreamer.filter(_.behaviorProtocol.protocolName == ProtocolName.ALT).map(_.getID.toInt)
    freeRiders = allNodesExceptStreamer.filter(_.behaviorProtocol.protocolName == ProtocolName.FR).map(_.getID.toInt)
  }
}