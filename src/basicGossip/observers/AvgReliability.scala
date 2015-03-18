package basicGossip.observers

import basicGossip.node.Usernode
import basicGossip.protocols.BasicGossip
import peersim.core.Network
import peersim.config.FastConfig
import peersim.core.Linkable
import hyparview.HyParViewJoinTest
import basicGossip.protocols.Link
import basicGossip.oracle.Oracle

class AvgReliability(name: String) extends BasicGossipObserver(name) {

  override def run: Boolean = {
    dumpAvgReliability
    reliabilityAbovePercentage
    dumpNodesWithZeroMessages
    altruisticReliability
    freeriderReliability
    false
  }

  def dumpAvgReliability = {
    val percentages = Oracle.allNodesExceptStreamer map {
      node => (node.messageList filter (x => x != null)).size.toFloat / BasicGossip.cycles
    }
    println("Reliability1: " + percentages.sum / percentages.size)
  }

  def reliabilityAbovePercentage = {
    val percentages = Oracle.allNodesExceptStreamer map {
      node => (node.messageList filter (x => x != null)).size.toFloat / BasicGossip.cycles
    }
    val aboveScore = percentages filter (_ > 0.98)
    println("Reliability2: " + aboveScore.size.toFloat / Network.size)

  }

  def dumpNodesWithZeroMessages = {
    val isolatedNodes = Oracle.allNodesExceptStreamer filter {
      un => un.messageList.filter { x => x != null } . size == 0
    }
    println("Amount of nodes with 0%: " + isolatedNodes.size)

    val protocolList = Oracle.nodesHpvProtocolExceptStreamer map (_._2)
    val b = protocolList map {
      prot =>
        val neigh = prot.neighbors filter (_.getID != 0)
        val neighIds = neigh map (_.getID)
        neighIds diff Oracle.freeRiders toSeq
    }
    val isolated = b.count { x => x.isEmpty }
    println("Isolated nodes number: " + isolated)
  }

  def dumpConnections = {
    Oracle.nodesHpvProtocol.map {
      elems =>
        print("Node: " + elems._1.getID + " -> ")
        val a = elems._2.neighbors
        a.map(x => print(x.getID + " "))
        println
    }
  }

  def altruisticReliability = {
    val percentages = Oracle.altruistics map {
      id =>
        (Oracle.getNode(id).messageList filter (x => x != null)).size.toFloat / BasicGossip.cycles

    }
    val aboveScore = percentages filter (_ > 0.98)
    println("Reliability of altruistics (>0.98%): " + aboveScore.size.toFloat / Oracle.altruistics.size)
  }

  def freeriderReliability = {
    val percentages = Oracle.freeRiders map {
      id =>
        (Oracle.getNode(id).messageList filter (x => x != null)).size.toFloat / BasicGossip.cycles

    }
    val abovefifty = percentages filter (_ > 0.50)
    println("Reliability of Free Riders above 50%: " + abovefifty.size.toFloat / Oracle.frAmount)

    val abovescore = percentages filter (_ > 0.98)
    println("Reliability of Free Riders above 98%: " + abovescore.size.toFloat / Oracle.frAmount)

  }

}