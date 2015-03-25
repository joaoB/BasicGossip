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
    dumpFreeRiders
    false
  }

  def dumpAvgReliability = {
    val percentages = Oracle.allNodesExceptStreamer map {
      node => (node.messageList filter (_.isDefined)).size.toFloat / BasicGossip.cycles
    }
    println("Reliability1: " + percentages.sum / percentages.size)
  }

  def reliabilityAbovePercentage = {
    val percentages = Oracle.allNodesExceptStreamer map {
      node => (node.messageList filter (_.isDefined)).size.toFloat / BasicGossip.cycles
    }
    val aboveScore = percentages filter (_ > 0.98)
    println("Reliability2: (all > 98%) " + aboveScore.size.toFloat / percentages.size)

  }

  def dumpNodesWithZeroMessages = {
    val isolatedNodes = Oracle.allNodesExceptStreamer filter {
      un => un.messageList.filter(_.isDefined).size == 0
    }
    println("Amount of nodes with 0%: " + isolatedNodes.size)

    val protocolList = Oracle.nodesHpvProtocolExceptStreamer map (_._2)

    val c = Oracle.allNodesExceptStreamer map {
      node =>
        val neigh = node.scoreList.keys filter (_ != 0)
        (neigh toSeq) diff Oracle.freeRiders toSeq
    }

    /* val b = protocolList map {
      prot =>
        val neigh = prot.neighbors filter (_.getID != 0)
        val neighIds = neigh map (_.getID)
        neighIds diff Oracle.freeRiders toSeq
    }*/
    /*println("NODES WITH 0 CONNECTION - " + c.count(_.size == 0))
    println("NODES WITH 1 CONNECTION - " + c.count(_.size == 1))
    println("NODES WITH 2 CONNECTION - " + c.count(_.size == 2))
    println("NODES WITH 3 CONNECTION - " + c.count(_.size == 3))
    println("NODES WITH 4 CONNECTION - " + c.count(_.size == 4))
    println("NODES WITH 5 CONNECTION - " + c.count(_.size == 5))
    println("NODES WITH 6 CONNECTION - " + c.count(_.size == 6))
    println("NODES WITH 7 CONNECTION - " + c.count(_.size == 7))
    println("NODES WITH 8 CONNECTION - " + c.count(_.size == 8))
    println("NODES WITH 9 CONNECTION - " + c.count(_.size == 9))
    println("NODES WITH 10 CONNECTION - " + c.count(_.size == 10))*/

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
        (Oracle.getNode(id).messageList.count(_.isDefined)).toFloat / BasicGossip.cycles
    }
    val aboveScore = percentages filter (_ > 0.98)
    println("Reliability of altruistics (>0.98%): " + aboveScore.size.toFloat / Oracle.altruistics.size)
  }

  def freeriderReliability = {
    val percentages = Oracle.freeRiders map {
      id =>
        (Oracle.getNode(id).messageList filter (_.isDefined)).size.toFloat / BasicGossip.cycles

    }
    val abovefifty = percentages filter (_ > 0.50)
    println("Reliability of Free Riders above 50%: " + abovefifty.size.toFloat / Oracle.frAmount)

    val abovescore = percentages filter (_ > 0.98)
    println("Reliability of Free Riders above 98%: " + abovescore.size.toFloat / Oracle.frAmount)

  }

  def dumpFreeRiders = {

    /* Oracle.allNodes map {
      node =>
        print("Node " + node.getID + "-> ")
        node.scoreList map {
          x => print(x._1 + " ")
        }
        println
        
        

    }*/

    //    Oracle.allNodesExceptStreamer map {
    //      elem => println(elem)
    //    }

  }

  /* Oracle.allNodes map {
      case node if !node.dumpFreeRiders.isEmpty => 
        print("Node " + node.getID + "-> ")
        node.dumpFreeRiders map {
          x => print(x + " ")
        }
        println
      case _ =>
    }
  }*/

}