package basicGossip.oracle

import hyparview.MyHyParView
import peersim.core.Network
import basicGossip.node.Usernode
import basicGossip.protocols.AltruisticProtocol
import scala.util.Random
import basicGossip.protocols.GeneralProtocol
import basicGossip.protocols.FRProtocol
import peersim.config.Configuration
import basicGossip.protocols.RationalProtocol
import basicGossip.protocols.ProtocolName

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

      val connect = Oracle.getNode(Random.nextInt(Network.size))
      val lst = (1 until Network.size toList).diff(List(nodeID)).diff(node.scoreList.keySet toList)

      lst match {
        case Nil => None
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
    val node = addNodeAux(new RationalProtocol("Free Rider"))
  }
  
  def injectFreeRiders = {
    val amount = 300
    val fr = Random.shuffle((1 until 999).toList).take(amount)
    allNodesExceptStreamer filter (node => fr.contains(node.getID)) map {
      x =>
        x.setProtocol(0, new RationalProtocol("free rider injected"))
        x.dropConnections
    }
    altruistics = allNodesExceptStreamer.filter(_.behaviorProtocol.protocolName == ProtocolName.ALT).map(_.getID.toInt)
    freeRiders = allNodesExceptStreamer.filter(_.behaviorProtocol.protocolName == ProtocolName.FR).map(_.getID.toInt)
  }
}