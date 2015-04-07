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
    newNodesReliability
    false
  }

  def newNodesReliability = {
    try {
      val node = Oracle.getNode(1000)
      val a = (node.messageList takeRight (BasicGossip.cycles - 500) filter (_.isDefined)).size.toFloat / (BasicGossip.cycles - 500)
      println("Reliability new node: " + a)
    } catch {
      case t: Throwable => // t.printStackTrace() // TODO: handle error
    }
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

    val c = Oracle.freeRiders map {
      node =>
        Oracle.getLinkable(node).neigh
    }

    val d = Oracle.altruistics map {
      node =>
        Oracle.getLinkable(node).neigh
    }

    val e = Oracle.altruistics map {
      node =>
        Oracle.getLinkable(node).neigh map {
          x => x.getID
        } filter (_ != 0) intersect Oracle.freeRiders
    }

    println("ALTRUISTICS CONNECTED TO 0 FR - " + e.count(_.size == 0))
    println("ALTRUISTICS CONNECTED TO 1 FR - " + e.count(_.size == 1))
    println("ALTRUISTICS CONNECTED TO 2 FR - " + e.count(_.size == 2))
    println("ALTRUISTICS CONNECTED TO 3 FR - " + e.count(_.size == 3))
    println("ALTRUISTICS CONNECTED TO 4 FR - " + e.count(_.size == 4))

    val f = Oracle.freeRiders map {
      node =>
        Oracle.getLinkable(node).neigh map (_.getID) filter (_ != 0) intersect Oracle.freeRiders
    }

    println

    println("FR CONNECTED TO 0 FR - " + f.count(_.size == 0))
    println("FR CONNECTED TO 1 FR - " + f.count(_.size == 1))
    println("FR CONNECTED TO 2 FR - " + f.count(_.size == 2))
    println("FR CONNECTED TO 3 FR - " + f.count(_.size == 3))
    println("FR CONNECTED TO 4 FR - " + f.count(_.size == 4))
    println("FR CONNECTED TO 5 FR - " + f.count(_.size == 5))
    println("FR CONNECTED TO 6 FR - " + f.count(_.size == 6))
    println

    val g = Oracle.altruistics map {
      node =>
        Oracle.getLinkable(node).neigh map (_.getID) filter (_ != 0) intersect Oracle.altruistics
    }

    println

    println("ALT CONNECTED TO 0 ALT  - " + g.count(_.size == 0))
    println("ALT CONNECTED TO 1 ALT  - " + g.count(_.size == 1))
    println("ALT CONNECTED TO 2 ALT  - " + g.count(_.size == 2))
    println("ALT CONNECTED TO 3 ALT  - " + g.count(_.size == 3))
    println("ALT CONNECTED TO 4 ALT  - " + g.count(_.size == 4))
    println("ALT CONNECTED TO 5 ALT  - " + g.count(_.size == 5))
    println("ALT CONNECTED TO 6 ALT  - " + g.count(_.size == 6))
    println("ALT CONNECTED TO 7 ALT  - " + g.count(_.size == 7))
    println("ALT CONNECTED TO 8 ALT  - " + g.count(_.size == 8))
    println("ALT CONNECTED TO 9 ALT  - " + g.count(_.size == 9))
    println

    println(Oracle.freeRiders diff Oracle.kicked.keySet.toSeq)
    println(" false positives : " + Oracle.badKicked)

    println("FREE RIDERS WITH 0 CONNECTION - " + c.count(_.size == 0))
    println("FREE RIDERS WITH 1 CONNECTION - " + c.count(_.size == 1))
    println("FREE RIDERS WITH 2 CONNECTION - " + c.count(_.size == 2))
    println("FREE RIDERS WITH 3 CONNECTION - " + c.count(_.size == 3))
    println("FREE RIDERS WITH 4 CONNECTION - " + c.count(_.size == 4))
    println
    println("ALTRUISTICS WITH 0 CONNECTION - " + d.count(_.size == 0))
    println("ALTRUISTICS WITH 1 CONNECTION - " + d.count(_.size == 1))
    println("ALTRUISTICS WITH 2 CONNECTION - " + d.count(_.size == 2))
    println("ALTRUISTICS WITH 3 CONNECTION - " + d.count(_.size == 3))
    println("ALTRUISTICS WITH 4 CONNECTION - " + d.count(_.size == 4))
    println("ALTRUISTICS WITH 5 CONNECTION - " + d.count(_.size == 5))
    println("ALTRUISTICS WITH 6 CONNECTION - " + d.count(_.size == 6))
    println("ALTRUISTICS WITH 7 CONNECTION - " + d.count(_.size == 7))
    println("ALTRUISTICS WITH 8 CONNECTION - " + d.count(_.size == 8))
    println("ALTRUISTICS WITH 9 CONNECTION - " + d.count(_.size == 9))
    println("ALTRUISTICS WITH 10 CONNECTION - " + d.count(_.size == 10))
    println("ALTRUISTICS WITH 11 CONNECTION - " + d.count(_.size == 11))
    println("ALTRUISTICS WITH 12 CONNECTION - " + d.count(_.size == 12))
    println("ALTRUISTICS WITH 13 CONNECTION - " + d.count(_.size == 13))
    println("ALTRUISTICS WITH 14 CONNECTION - " + d.count(_.size == 14))

    val r = Oracle.freeRiders map {
      node =>
        Oracle.getLinkable(node).neigh map (_.getID) filter (_ != 0) intersect Oracle.altruistics
    }
    
    println
    
    println("FREE RIDERS WITH 0 ALTRUISTICS - " + r.count(_.size == 0))
    println("FREE RIDERS WITH 1 ALTRUISTICS - " + r.count(_.size == 1))
    println("FREE RIDERS WITH 2 ALTRUISTICS - " + r.count(_.size == 2))
    println("FREE RIDERS WITH 3 ALTRUISTICS - " + r.count(_.size == 3))
    println("FREE RIDERS WITH 4 ALTRUISTICS - " + r.count(_.size == 4))
   

    println("FR challenges " + Oracle.FRchallenges / Oracle.frAmount)
    println("altruistic challenges " + Oracle.altruisticChallanges / Oracle.altruistics.size)
    /*  println("NODES WITH 5 CONNECTION - " + c.count(_.size == 5))
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

    val abovescore = percentages filter (_ > 0.9)
    println("Reliability of Free Riders above 90%: " + abovescore.size.toFloat / Oracle.frAmount)

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

object AvgReliability extends AvgReliability("mamanavara")