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
    //dumpScores
    //   simulationData
    false
  }

  def dumpScores = {
    Oracle.allNodes map {
      node =>
        print("Node: " + node.getID + " -> ")
        node.scoreList map {
          score => print(score._1 + " <-> " + score._2 + " || ")
        }
        println
    }
  }

  def newNodesReliability = {
    //    try {
    //      val node = Oracle.getNode(1000)
    //      val a = (node.messageList takeRight (BasicGossip.cycles - 500) filter (_.isDefined)).size.toFloat / (BasicGossip.cycles - 500)
    //      println("Reliability new node: " + a)
    //    } catch {
    //      case t: Throwable => // t.printStackTrace() // TODO: handle error
    //    }
  }

  def dumpAvgReliability = {
    val percentages = Oracle.allNodesExceptStreamer map {
      //  node => (node.messageList filter (_.isDefined)).size.toFloat / BasicGossip.cycles
      node => node.messageList.size.toFloat / BasicGossip.cycles

    }
    println("Reliability1: " + percentages.sum / percentages.size)
  }

  def reliabilityAbovePercentage = {
    val percentages = Oracle.allNodesExceptStreamer map {
      //  node => (node.messageList filter (_.isDefined)).size.toFloat / BasicGossip.cycles
      node => node.messageList.size.toFloat / BasicGossip.cycles

    }
    val aboveScore = percentages filter (_ > 0.98)
    println("Reliability2: (all > 98%) " + aboveScore.size.toFloat / percentages.size)

  }

  def dumpNodesWithZeroMessages = {
    val isolatedNodes = Oracle.allNodesExceptStreamer filter {
      //  un => un.messageList.filter(_.isDefined).size == 0
      un => un.messageList.size == 0

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

    //    println
    //
    //    println("ALT CONNECTED TO 0 ALT  - " + g.count(_.size == 0))
    //    println("ALT CONNECTED TO 1 ALT  - " + g.count(_.size == 1))
    //    println("ALT CONNECTED TO 2 ALT  - " + g.count(_.size == 2))
    //    println("ALT CONNECTED TO 3 ALT  - " + g.count(_.size == 3))
    //    println("ALT CONNECTED TO 4 ALT  - " + g.count(_.size == 4))
    //    println("ALT CONNECTED TO 5 ALT  - " + g.count(_.size == 5))
    //    println("ALT CONNECTED TO 6 ALT  - " + g.count(_.size == 6))
    //    println("ALT CONNECTED TO 7 ALT  - " + g.count(_.size == 7))
    //    println("ALT CONNECTED TO 8 ALT  - " + g.count(_.size == 8))
    //    println("ALT CONNECTED TO 9 ALT  - " + g.count(_.size == 9))
    //    println

    println("    num of ninja riders " + (Oracle.frAmount - Oracle.kicked.size))
    println("    false positives : " + Oracle.badKicked.size)

    println("FREE RIDERS WITH 0 CONNECTION - " + c.count(_.size == 0))
    println("FREE RIDERS WITH 1 CONNECTION - " + c.count(_.size == 1))
    println("FREE RIDERS WITH 2 CONNECTION - " + c.count(_.size == 2))
    println("FREE RIDERS WITH 3 CONNECTION - " + c.count(_.size == 3))
    println("FREE RIDERS WITH 4 CONNECTION - " + c.count(_.size == 4))
    println("FREE RIDERS WITH 5 CONNECTION - " + c.count(_.size == 5))
    println("FREE RIDERS WITH 6 CONNECTION - " + c.count(_.size == 6))
    println("FREE RIDERS WITH 7 CONNECTION - " + c.count(_.size == 7))
    println("FREE RIDERS WITH 8 CONNECTION - " + c.count(_.size == 8))
    println("FREE RIDERS WITH 9 CONNECTION - " + c.count(_.size == 9))
    println("FREE RIDERS WITH 10 CONNECTION - " + c.count(_.size == 10))
    println("FREE RIDERS WITH 11 CONNECTION - " + c.count(_.size == 11))
    println("FREE RIDERS WITH 12 CONNECTION - " + c.count(_.size == 12))
    println("FREE RIDERS WITH 13 CONNECTION - " + c.count(_.size == 13))
    println("FREE RIDERS WITH > 13 CONNECTION - " + c.count(_.size > 13))

    //    Oracle.nodesHpvProtocol map {
    //      x => println(x._2.neighbors().size)
    //    }

    println
    //    println("ALTRUISTICS WITH 0 CONNECTION - " + d.count(_.size == 0))
    //    println("ALTRUISTICS WITH 1 CONNECTION - " + d.count(_.size == 1))
    //    println("ALTRUISTICS WITH 2 CONNECTION - " + d.count(_.size == 2))
    //    println("ALTRUISTICS WITH 3 CONNECTION - " + d.count(_.size == 3))
    //    println("ALTRUISTICS WITH 4 CONNECTION - " + d.count(_.size == 4))
    //    println("ALTRUISTICS WITH 5 CONNECTION - " + d.count(_.size == 5))
    //    println("ALTRUISTICS WITH 6 CONNECTION - " + d.count(_.size == 6))
    //    println("ALTRUISTICS WITH 7 CONNECTION - " + d.count(_.size == 7))
    //    println("ALTRUISTICS WITH 8 CONNECTION - " + d.count(_.size == 8))
    //    println("ALTRUISTICS WITH 9 CONNECTION - " + d.count(_.size == 9))
    //    println("ALTRUISTICS WITH 10 CONNECTION - " + d.count(_.size == 10))
    //    println("ALTRUISTICS WITH 11 CONNECTION - " + d.count(_.size == 11))
    //    println("ALTRUISTICS WITH 12 CONNECTION - " + d.count(_.size == 12))
    //    println("ALTRUISTICS WITH 13 CONNECTION - " + d.count(_.size == 13))
    //    println("ALTRUISTICS WITH 14 CONNECTION - " + d.count(_.size == 14))
    //
    //    val r = Oracle.freeRiders map {
    //      node =>
    //        Oracle.getLinkable(node).neigh map (_.getID) filter (_ != 0) intersect Oracle.altruistics
    //    }
    //
    //    println
    //
    //    println("FREE RIDERS WITH 0 ALTRUISTICS - " + r.count(_.size == 0))
    //    println("FREE RIDERS WITH 1 ALTRUISTICS - " + r.count(_.size == 1))
    //    println("FREE RIDERS WITH 2 ALTRUISTICS - " + r.count(_.size == 2))
    //    println("FREE RIDERS WITH 3 ALTRUISTICS - " + r.count(_.size == 3))
    //    println("FREE RIDERS WITH 4 ALTRUISTICS - " + r.count(_.size == 4))
    //
    println("FR challenges " + Oracle.FRchallenges)
    println("altruistic challenges " + Oracle.altruisticChallanges)
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
        //    (Oracle.getNode(id).messageList.count(_.isDefined)).toFloat / BasicGossip.cycles
        Oracle.getNode(id).messageList.size.toFloat / BasicGossip.cycles

    }
    val aboveScore = percentages filter (_ > 0.98)
    println("Reliability of altruistics (>0.98%): " + aboveScore.size.toFloat / Oracle.altruistics.size)
  }

  def freeriderReliability = {
    val percentages = Oracle.freeRiders map {
      id =>
        //(Oracle.getNode(id).messageList count (_.isDefined)).toFloat / BasicGossip.cycles
        Oracle.getNode(id).messageList.size.toFloat / BasicGossip.cycles

    }

    val abovefive = percentages filter (_ > 0.05)
    println("Reliability of Free Riders above 5%: " + abovefive.size.toFloat / Oracle.frAmount)

    val aboveten = percentages filter (_ > 0.10)
    println("Reliability of Free Riders above 10%: " + aboveten.size.toFloat / Oracle.frAmount)

    val abovetwentyfive = percentages filter (_ > 0.15)
    println("Reliability of Free Riders above 15%: " + abovetwentyfive.size.toFloat / Oracle.frAmount)

    val aboveforthy = percentages filter (_ > 0.40)
    println("Reliability of Free Riders above 40%: " + aboveforthy.size.toFloat / Oracle.frAmount)

    val abovefifty = percentages filter (_ > 0.50)
    println("Reliability of Free Riders above 50%: " + abovefifty.size.toFloat / Oracle.frAmount)

    val abovescore = percentages filter (_ > 0.9)
    println("Reliability of Free Riders above 90%: " + abovescore.size.toFloat / Oracle.frAmount)

    println("Reliability of Free Riders above 90%: " + percentages.filter(_ > 0.98).size.toFloat / Oracle.frAmount)

    val altruistics = (1 - Oracle.frPercentage) * Network.size
    println("Avg Altruistic Message #: " + Oracle.altruisticsAmountOfSentMessages / altruistics)

    if (Oracle.frAmount != 0){
      println("Avg FR Message #: " + Oracle.frAmountOfSentMessages / Oracle.frAmount)
    }

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

  def simulationData = {
    val altReli = Oracle.altruistics map {
      id =>
        //(Oracle.getNode(id).messageList.count(_.isDefined)).toFloat / BasicGossip.cycles
        Oracle.getNode(id).messageList.size.toFloat / BasicGossip.cycles
    }

    print(altReli.filter(_ > 0.0).size.toFloat / Oracle.altruistics.size + "\t")
    print(altReli.filter(_ > 0.1).size.toFloat / Oracle.altruistics.size + "\t")
    print(altReli.filter(_ > 0.2).size.toFloat / Oracle.altruistics.size + "\t")
    print(altReli.filter(_ > 0.3).size.toFloat / Oracle.altruistics.size + "\t")
    print(altReli.filter(_ > 0.4).size.toFloat / Oracle.altruistics.size + "\t")
    print(altReli.filter(_ > 0.5).size.toFloat / Oracle.altruistics.size + "\t")
    print(altReli.filter(_ > 0.6).size.toFloat / Oracle.altruistics.size + "\t")
    print(altReli.filter(_ > 0.7).size.toFloat / Oracle.altruistics.size + "\t")
    print(altReli.filter(_ > 0.8).size.toFloat / Oracle.altruistics.size + "\t")
    print(altReli.filter(_ > 0.9).size.toFloat / Oracle.altruistics.size + "\t")
    print(altReli.filter(_ > 0.98).size.toFloat / Oracle.altruistics.size + "\t")

    val frReli = Oracle.freeRiders map {
      id =>
        //(Oracle.getNode(id).messageList count (_.isDefined)).toFloat / BasicGossip.cycles
        Oracle.getNode(id).messageList.size.toFloat / BasicGossip.cycles
    }

    print(frReli.filter(_ > 0.0).size.toFloat / Oracle.frAmount + "\t")
    print(frReli.filter(_ > 0.1).size.toFloat / Oracle.frAmount + "\t")
    print(frReli.filter(_ > 0.2).size.toFloat / Oracle.frAmount + "\t")
    print(frReli.filter(_ > 0.3).size.toFloat / Oracle.frAmount + "\t")
    print(frReli.filter(_ > 0.4).size.toFloat / Oracle.frAmount + "\t")
    print(frReli.filter(_ > 0.5).size.toFloat / Oracle.frAmount + "\t")
    print(frReli.filter(_ > 0.6).size.toFloat / Oracle.frAmount + "\t")
    print(frReli.filter(_ > 0.7).size.toFloat / Oracle.frAmount + "\t")
    print(frReli.filter(_ > 0.8).size.toFloat / Oracle.frAmount + "\t")
    print(frReli.filter(_ > 0.9).size.toFloat / Oracle.frAmount + "\t")

    print(Oracle.frAmount - Oracle.kicked.size + "\t")
    print(Oracle.badKicked.size + "\t")

    val e = Oracle.altruistics map {
      node =>
        Oracle.getLinkable(node).neigh map {
          x => x.getID
        } filter (_ != 0) intersect Oracle.freeRiders
    }

    print(e.count(_.size == 0) + "\t")
    print(e.count(_.size == 1) + "\t")
    print(e.count(_.size == 2) + "\t")
    print(e.count(_.size == 3) + "\t")
    print(e.count(_.size == 4) + "\t")
    print(e.count(_.size == 5) + "\t")
    print(e.count(_.size == 6) + "\t")
    print(e.count(_.size == 7) + "\t")
    print(e.count(_.size == 8) + "\t")
    print(e.count(_.size == 9) + "\t")
    print(e.count(_.size == 10) + "\t")

    print(Oracle.FRchallenges + "\t")
    print(Oracle.altruisticChallanges + "\t")

    print(Oracle.altruisticsAmountOfSentMessages + "\t")

    val altruistics = (1 - Oracle.frPercentage) * Network.size
    print(Oracle.altruisticsAmountOfSentMessages / altruistics + "\t")

    println
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