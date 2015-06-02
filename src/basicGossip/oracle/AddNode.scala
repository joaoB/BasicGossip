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

protected abstract class AddNode extends AllNodes {

  val frPercentage = Configuration.getDouble("Oracle.FR_PERCENTAGE")
  var altruistics: List[Int] = List[Int]()
  var simpleJoins = 0
  var total = 1 until Network.size toList
  val frAmount = (Network.size * frPercentage).toInt
  var freeRiders = Random.shuffle(total).take(frAmount)

  private def addNodeAux(protocol: GeneralProtocol): Usernode = {
    val n = new Usernode("Usernode")
    Network.add(n)
    val node = Oracle.getNode(n.getID.toInt)
    node.setProtocol(0, protocol)

    val nodeID = node.getID.toInt
    val nodeNode = Network.get(nodeID)

    for (id <- 0 until protocol.maxWin) {
      simpleJoins += 1

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
    altruistics = altruistics.::(node.getID.toInt)
  }

  def addFreeRider = {
    val node = addNodeAux(new FRProtocol("Free Rider"))
    freeRiders = freeRiders.::(node.getID.toInt)
  }
  
  def addRational = {
    val node = addNodeAux(new RationalProtocol("Free Rider"))
    freeRiders = freeRiders.::(node.getID.toInt)  
  }

  def injectFreeRiders = {
    val amount = 300
    freeRiders = Random.shuffle(1 until 1000 toList).take(amount)
    total = (1 until Network.size).toList
    altruistics = total diff freeRiders
    allNodesExceptStreamer filter (node => freeRiders.contains(node.getID)) map {
      x => x.setProtocol(0, new FRProtocol("free rider injected"))
    }
  }
}