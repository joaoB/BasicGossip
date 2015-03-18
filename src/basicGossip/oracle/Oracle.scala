package basicGossip.oracle

import basicGossip.messages.Info
import basicGossip.node.Usernode
import scala.collection.mutable.MutableList
import peersim.config.Configuration
import peersim.core.Network
import hyparview.HyParViewJoinTest
import peersim.config.FastConfig
import basicGossip.protocols.Link
import scala.util.Random
import basicGossip.protocols.BasicGossip

//Oracle has an eye on everythinggi
class Oracle {

  val frPercentage = Configuration.getDouble("Oracle." + "FR_PERCENTAGE")
  val maxHops = Configuration.getInt("Oracle." + "MAX_HOPS")
  val peerAlgorithm =  Configuration.getInt("Oracle." + "PEER_ALGORITHM")
  val fanout = Configuration.getInt("Oracle.FANOUT", BasicGossip.fanout)
  
  val total = 1 until Network.size toList
  val frAmount = (Network.size * frPercentage).toInt
  val freeRiders = Random.shuffle(total).take(frAmount)
  val altruistics = total diff freeRiders

  var maxHopInfo: Option[Info] = None
  var amountOfSentMessages: Int = 0
  var avgHops = MutableList[Int]()

  def saveMaxHopInfo(info: Info) {
    maxHopInfo match {
      case Some(elem) if info.hop > elem.hop => updateMaxHopInfo(info)
      case None => updateMaxHopInfo(info)
      case _ =>
    }
  }

  def incSentMessages = amountOfSentMessages += 1

  private def updateMaxHopInfo(info: Info) {
    maxHopInfo = Some(info)
  }

  def saveHop(info: Info) = {
    avgHops.+=(info.hop)
  }

  private def allNodesAux(start: Int): List[Usernode] =
    (for (i <- start until Network.size) yield Network.get(i)).map {
      case un: Usernode => un
    } toList

  def allNodesExceptStreamer = allNodesAux(1)

  def allNodes = allNodesAux(0)

  def nodesHpvProtocolExceptStreamer: List[(Usernode, HyParViewJoinTest)] = {
    nodesHpvProtocol(allNodesExceptStreamer)
  }

  def nodesHpvProtocol: List[(Usernode, HyParViewJoinTest)] =
    nodesHpvProtocol(Oracle.allNodes)

  def nodesHpvProtocol(nodes: List[Usernode]): List[(Usernode, HyParViewJoinTest)] = {
    nodes map {
      node =>
        node.getProtocol(HyParViewJoinTest.protocolID) match {
          case prot: HyParViewJoinTest => (node, prot)
        }
    } toList
  }

  def nodeHpvProtocol(id: Int): (Usernode, HyParViewJoinTest) = {
    nodesHpvProtocol(List(getNode(id))).head
  }

  def getLinkable(usernode: Usernode): Link =
    usernode.getProtocol(FastConfig.getLinkable(0)) match {
      case link: Link => link
    }

  def getNode(id: Int): Usernode = {
    Network.get(id) match {
      case un: Usernode => un
    }
  }

}

object Oracle extends Oracle