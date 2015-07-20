package basicGossip.observers

import basicGossip.node.Usernode
import basicGossip.protocols.BasicGossip
import peersim.core.Network
import peersim.config.FastConfig
import peersim.core.Linkable

import basicGossip.protocols.Link
import basicGossip.oracle.Oracle
import basicGossip.node.NodeStatus

class AvgReliability(name: String) extends BasicGossipObserver(name) {

  override def run: Boolean = {
    //dumpAvgReliability
    //reliabilityAbovePercentage
    //dumpNodesWithZeroMessages

    altruisticReliability
    freeriderReliability

    //dumpFreeRiders
    //newNodesReliability
    // dumpScores
    //simulationData
    //dumpReliability
    //a
    //thisone
    //println
    //b
    //c
    //d
    //altReliLastRounds
    //falsePos
    //printScore
    //disconnects
    //isolatedNodes
    //maxmin

    //    aalt
    //    afr
    //    hopsNavg
    false
  }

  def aalt = {
    val last = Oracle.altruistics map {
      id => Oracle.getNode(id).messageList.filter(_ >= BasicGossip.cycles - 5000).size.toFloat / 5000
    }
    println("alt reli: " + last.sum.toFloat / last.size)
    println("alt messages " + Oracle.altruisticsAmountOfSentMessages)

  }
  def afr = {
    val last = Oracle.freeRiders map {
      id => Oracle.getNode(id).messageList.filter(_ >= BasicGossip.cycles - 500).size.toFloat / 500
    }
    println("fr reli: " + last.sum.toFloat / last.size)

    println("fr messages : " + Oracle.frAmountOfSentMessages)
  }

  def hopsNavg = {
    println("max hops: " + Oracle.maxHopInfo)
    println("avg: " + Oracle.avgHops.sum.toFloat / Oracle.avgHops.size)
  }

  def maxmin = {
    val a = Oracle.allNodesExceptStreamer map {
      node =>
        val asd = node.scoreList.filter(_._2.status == NodeStatus.ACTIVE).size
        //        if (asd > 15) {
        //          println(node.scoreList)
        //          println(node.solvingChallenges)
        //        }
        asd
    }
    try {
      println("MAX " + a.max)

      println("MIN " + a.min)
    } catch {
      case e =>
    }
  }

  def disconnects = {
    println("disconnects " + Oracle.disconnects)
  }

  def isolatedNodes = {
    val isolatedNodes = Oracle.allNodesExceptStreamer filter {
      un => un.scoreList.keySet.filter(_ != 0).toList.diff(Oracle.freeRiders).isEmpty

    }

    println("alt puzzles " + Oracle.altruisticChallanges)
    println("isolated nodes: " + isolatedNodes.size)
    val a = isolatedNodes.sortBy { x => x.scoreList.size }
    val sorted = a map (_.scoreList.size)
    val nodesIds = a map (_.getID)
    println("isolated nodes ids -> " + a)
    println("isolated nodes MIN connections: " + sorted.reduceOption(_ min _))
  }

  def printScore = {
    try {
      val score = Oracle.getNode(1).scoreList.filter(_._2.status == NodeStatus.ACTIVE) map {
        a =>
          val score = a
          print(a._1 + " > " + a._2.score + " || \t")
      }
      println
    } catch {
      case e: Throwable =>
    }
  }

  def falsePos = println(Oracle.badKicked.size)

  def altReliLastRounds = {
    val percentages = Oracle.altruistics map {
      id =>
        Oracle.getNode(id).messageList.filter(_ > Oracle.currentPackage - 50).size.toFloat / 50

    }

    println("alt reli last 50 rounds " + percentages.sum / Oracle.altruistics.size)

  }

  def d = {
    val a = Oracle.altruistics map {
      id =>
        val a = Oracle.getNode(id).scoreList.filter(elem => Oracle.altruistics.contains(elem._1))
        //println(a.size.toFloat  + " -> " + Oracle.getNode(id).scoreList.size.toFloat)
        a.size.toFloat / Oracle.getNode(id).scoreList.size
    }
    val b = a.filter(!_.isNaN())

    println("A " + Oracle.currentPackage + " -> " + +b.sum.toFloat / Oracle.altruistics.size)
  }

  def c = {
    println("false positives : " + Oracle.badKicked.size)

    println("challengesBeforeStream -> " + Oracle.challengesBeforeStream)
    println("disconnectsBeforeStream -> " + Oracle.disconnectsBeforeStream)

    println(Oracle.currentPackage + " -> " + "num of ninja riders " + (Oracle.frAmount - Oracle.kicked.size))

    val alt = Oracle.altruistics map {
      id =>
        //    (Oracle.getNode(id).messageList.count(_.isDefined)).toFloat / BasicGossip.cycles
        Oracle.getNode(id).messageList.size.toFloat / BasicGossip.cycles
    }

    val fr = Oracle.freeRiders map {
      id =>
        //    (Oracle.getNode(id).messageList.count(_.isDefined)).toFloat / BasicGossip.cycles
        Oracle.getNode(id).messageList.size.toFloat / BasicGossip.cycles
    }

    println("total messages: " + Oracle.altruisticsAmountOfSentMessages)
    println("alt messages: " + Oracle.altruisticsAmountOfSentMessages.toFloat / Oracle.altruistics.size)
    println("fr messages: " + Oracle.frAmountOfSentMessages.toFloat / Oracle.freeRiders.size)

    println("alt crypto " + Oracle.altruisticChallanges / Oracle.altruistics.size)
    println("fr crypto " + Oracle.FRchallenges)
    //println("fr score size " + Oracle.getNode(Oracle.freeRiders.head).scoreList.size)
    println("kicked " + Oracle.kicked)

    val aw = Oracle.altruistics.map({
      id => Oracle.getNode(id).scoreList.size
    })
    println("alt min connections " + aw.min)

    println("alt max connections " + aw.max)

    println("badkicked " + Oracle.badKicked.filter(_._2 > 2).size)

    /*val awp = Oracle.freeRiders.map({
      id => Oracle.getNode(id).scoreList.size
    })

    println("fr min connections " + awp.min)

    println("fr max connections " + awp.max)

    Oracle.getNode(Oracle.freeRiders.head).scoreList.keySet map {
      x =>
        val a = Oracle.freeRiders.contains(x)
        println(a)
    }
    */

    //val a = (Oracle.freeRiders map {
    //   id => Oracle.getNode(id).scoreList.size
    //})

    //println("MAX FREE RIDERS CONNECTIONS: " + a.max)

    val isolatedNodes = Oracle.allNodesExceptStreamer filter {
      un => un.scoreList.keySet.filter(_ != 0).toList.diff(Oracle.freeRiders).isEmpty

    }

    println("isolated nodes: " + isolatedNodes.size)

    //println("messages amount: " + Oracle.altruisticsAmountOfSentMessages)
    //   val a = Oracle.allNodes.filter(_.scoreList.size < Oracle.MIN_WIN_TO_SEARCH).size
    //    println("NODES ABAIXO " + a)
    //println("head score list size " + Oracle.getNode(Oracle.freeRiders.head).scoreList.size)
    //    println("Max hops -> " + Oracle.maxHopInfo)
    //
    //    Oracle.avgHops.size match {
    //      case 0 =>
    //      case size => println("Avg hops " + Oracle.avgHops.sum / size)
    //    }
    //
    //        val a = Oracle.altruistics map {
    //          id =>
    //            val a = Oracle.getNode(id).scoreList.filter(elem => Oracle.altruistics.contains(elem._1) )
    //            a.size.toFloat / Oracle.getNode(id).scoreList.size.toFloat
    //        }
    // println("cicle " + Oracle.currentPackage)
    //  
    // val b = a.filter(!_.isNaN())
    //val b = List(1.0, 0.8888889, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.8333333, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.9, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.33333334, 1.0, 0.875, 1.0, 1.0, 1.0, 1.0, NaN, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.7777778, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.9, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.9, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.90909094, 1.0, 1.0, 1.0, 0.8888889, 1.0, 1.0, 1.0, 0.85714287, 1.0, 1.0, 1.0, 1.0, 1.0, 0.9, 1.0, 1.0, 1.0, 0.85714287, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.9, 1.0, 1.0, 0.8333333, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.9, 1.0, 1.0, 1.0, 1.0, 0.9, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.85714287, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.8333333, 1.0, 1.0, 1.0, 0.33333334, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.8, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.8888889, 0.90909094, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.8888889, 1.0, 1.0, 0.875, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.875, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.8333333, 1.0, 1.0, 0.9, 1.0, 0.5, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.8333333, 1.0, 1.0, 0.8, 1.0, 1.0, 1.0, 1.0, 1.0, 0.9, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.9, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.8888889, 1.0, 1.0, 1.0, 1.0, 1.0, 0.9, 1.0, 1.0, 1.0, 0.875, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.9166667, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.8888889, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.875, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.8888889, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.85714287, 1.0, 1.0, 1.0, 1.0, 1.0, 0.8, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.875, 1.0, 0.85714287, 1.0, 1.0, 1.0, 0.8333333, 1.0, 1.0, 1.0, 1.0, 1.0, 0.85714287, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.8888889, 1.0, 1.0, 1.0, 1.0, 1.0, 0.8888889, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.9, 0.875, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.9166667, 0.9, 1.0, 1.0, 0.9, 0.9, 0.9, 0.8, 0.8888889)

    //println("a-> " + b.sum.toDouble)
    //println("A " + b.sum.toFloat / Oracle.altruistics.size)

    //println("FR CRYPTO: " + Oracle.FRchallenges)
    //println("FR CONNECTIONS  " + Oracle.getNode(Oracle.freeRiders.head).scoreList.keys.size)

    /*println("ALT CRYPTO: " + Oracle.altruisticChallanges / 999)

    println("messages amount: " + Oracle.altruisticsAmountOfSentMessages)

    println("false positives : " + Oracle.badKicked.filter(_._2 > 2).size)

    val percentages = Oracle.allNodesExceptStreamer map {
      //  node => (node.messageList filter (_.isDefined)).size.toFloat / BasicGossip.cycles
      node => node.messageList.size.toFloat / BasicGossip.cycles

    }
    println("Reliability1: " + percentages.sum / percentages.size)

    try {
      val a = (for (a <- 1000 until 1001) yield Oracle.getNode(a).messageList.size.toFloat / 500)
      println("percentagesNew: " + a.sum / a.size)
    } catch {
      case x =>
    }
*/
    //
    //    val e = Oracle.altruistics map {
    //      node =>
    //        Oracle.getLinkable(node).neigh map {
    //          x => x.getID
    //        } filter (_ != 0) intersect Oracle.freeRiders
    //    }
    //
    //    println("ALTRUISTICS CONNECTED TO 0 FR - " + e.count(_.size == 0))
    //    
    //    println(Oracle.getNode(Oracle.freeRiders.head).scoreList.size)
    //    println(Oracle.getNode(Oracle.freeRiders.head).messageList.size)

    // println(Oracle.currentPackage + " -> " + "num of ninja riders " + (Oracle.frAmount - Oracle.kicked.size))

    //    val isolatedNodes = Oracle.allNodesExceptStreamer filter {
    //      un => un.scoreList.keySet.filter(_ != 0).toList.diff(Oracle.freeRiders).isEmpty
    //
    //    }

    //    val isolatedNodes = Oracle.nodesHpvProtocolExceptStreamer filter {
    //      un =>
    //        if (un._1.getID % 5000 == 0) 
    //          println(un._1.getID)
    //        un._2.neighbors.map(_.getID.toInt).diff(Oracle.freeRiders).isEmpty
    //
    //    }

    //    Oracle.getNode(445).scoreList map {
    //      x => print(x._2 + " \t")
    //    }
    //    println

    //println("FR CRYPTO: " + Oracle.FRchallenges)
    //println("ALT CRYPTO: " + Oracle.altruisticChallanges)
    // println("isolated nodes: " + isolatedNodes.size)
    //println("messages amount: " + Oracle.altruisticsAmountOfSentMessages)

    //    for (id <- 1 to 10) {
    //      nodeScore(id)
    //    }

    //println("false positives : " + Oracle.badKicked.filter(_._2 > 5).size)

    //nodeScore(500)

  }

  def nodeScore(id: Int) {
    //print("Node:" + id + "\t")
    //    val a = Oracle.getNode(id).scoreList.filter(_._1 != 0).head._2.toInt
    //
    //    println(a)
    //println(Oracle.getNode(a._1.toInt).scoreList(id))
    println
  }

  def b = {
    Oracle.allNodes map {
      node =>
        print("Node: " + node.getID + " -> ")
        node.scoreList.values map {
          x => print(x + "\t")
        }
        println
    }

  }

  def thisone = {
    val e = Oracle.altruistics map {
      node =>
        Oracle.getLinkable(node).neigh map {
          x => x.getID
        } filter (_ != 0) intersect Oracle.freeRiders
    }

    println("ALTRUISTICS CONNECTED TO 0 FR - " + e.count(_.size == 0))
    println("FR challenges " + Oracle.FRchallenges)
    println("altruistic challenges " + Oracle.altruisticChallanges)

  }

  def a = {
    println(Oracle.altruisticsAmountOfSentMessages / Oracle.altruistics.size)
  }

  def dumpReliability {

    val a = Oracle.altruistics map {
      nodeID => Oracle.getNode(nodeID).messageList.size.toFloat / BasicGossip.cycles
    }
    a.sortBy { x => -x } map {
      x => println(x)
    }

    println("////////////////////////////////////////////")

    val b = Oracle.freeRiders map {
      nodeID => Oracle.getNode(nodeID).messageList.size.toFloat / BasicGossip.cycles
    }

    b.sortBy { x => -x } map {
      x => println(x)
    }

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
    //println("Amount of nodes with 0%: " + isolatedNodes.size)

    //val protocolList = Oracle.nodesHpvProtocolExceptStreamer map (_._2)

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

    //    println("ALTRUISTICS CONNECTED TO 2 FR - " + e.count(_.size == 2))
    //    println("ALTRUISTICS CONNECTED TO 3 FR - " + e.count(_.size == 3))
    //    println("ALTRUISTICS CONNECTED TO 4 FR - " + e.count(_.size == 4))
    //    println("ALTRUISTICS CONNECTED TO 5 FR - " + e.count(_.size == 5))
    //    println("ALTRUISTICS CONNECTED TO 6 FR - " + e.count(_.size == 6))
    //    println("ALTRUISTICS CONNECTED TO 7 FR - " + e.count(_.size == 7))
    //    println("ALTRUISTICS CONNECTED TO 8 FR - " + e.count(_.size == 8))
    //    println("ALTRUISTICS CONNECTED TO 9 FR - " + e.count(_.size == 9))

    //    val f = Oracle.freeRiders map {
    //      node =>
    //        Oracle.getLinkable(node).neigh map (_.getID) filter (_ != 0) intersect Oracle.freeRiders
    //    }
    //
    //    println
    //
    //    println("FR CONNECTED TO 0 FR - " + f.count(_.size == 0))
    //    println("FR CONNECTED TO 1 FR - " + f.count(_.size == 1))
    //    println("FR CONNECTED TO 2 FR - " + f.count(_.size == 2))
    //    println("FR CONNECTED TO 3 FR - " + f.count(_.size == 3))
    //    println("FR CONNECTED TO 4 FR - " + f.count(_.size == 4))
    //    println("FR CONNECTED TO 5 FR - " + f.count(_.size == 5))
    //    println("FR CONNECTED TO 6 FR - " + f.count(_.size == 6))
    //    println

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
    //println("alt connected to > 4 alts" + g.count(_.size >= 4))
    //    println("ALT CONNECTED TO 5 ALT  - " + g.count(_.size == 5))
    //    println("ALT CONNECTED TO 6 ALT  - " + g.count(_.size == 6))
    //    println("ALT CONNECTED TO 7 ALT  - " + g.count(_.size == 7))
    //    println("ALT CONNECTED TO 8 ALT  - " + g.count(_.size == 8))
    //    println("ALT CONNECTED TO 9 ALT  - " + g.count(_.size == 9))
    //    println

    println("num of ninja riders " + (Oracle.frAmount - Oracle.kicked.size))
    //    println(Oracle.frAmount - Oracle.kicked.size)
    //
    println("false positives : " + Oracle.badKicked.size)

    //    println(Oracle.badKicked.size)

    //    println("FREE RIDERS WITH 0 CONNECTION - " + c.count(_.size == 0))
    //    println("FREE RIDERS WITH 1 CONNECTION - " + c.count(_.size == 1))
    //    println("FREE RIDERS WITH 2 CONNECTION - " + c.count(_.size == 2))
    //    println("FREE RIDERS WITH 3 CONNECTION - " + c.count(_.size == 3))
    //    println("FREE RIDERS WITH 4 CONNECTION - " + c.count(_.size == 4))
    //    println("FREE RIDERS WITH 5 CONNECTION - " + c.count(_.size == 5))
    //    println("FREE RIDERS WITH 6 CONNECTION - " + c.count(_.size == 6))
    //    println("FREE RIDERS WITH 7 CONNECTION - " + c.count(_.size == 7))
    //    println("FREE RIDERS WITH 8 CONNECTION - " + c.count(_.size == 8))
    //    println("FREE RIDERS WITH 9 CONNECTION - " + c.count(_.size == 9))
    //    println("FREE RIDERS WITH 10 CONNECTION - " + c.count(_.size == 10))
    //    println("FREE RIDERS WITH 11 CONNECTION - " + c.count(_.size == 11))
    //    println("FREE RIDERS WITH 12 CONNECTION - " + c.count(_.size == 12))
    //    println("FREE RIDERS WITH 13 CONNECTION - " + c.count(_.size == 13))
    //    println("FREE RIDERS WITH > 13 CONNECTION - " + c.count(_.size > 13))

    //    Oracle.nodesHpvProtocol map {
    //      x => println(x._2.neighbors().size)
    //    }

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
    //println("FR challenges " + Oracle.FRchallenges)
    //println("altruistic challenges " + Oracle.altruisticChallanges)

    //println(Oracle.FRchallenges)
    //println(Oracle.altruisticChallanges)

    /*  println("NODES WITH 5 CONNECTION - " + c.count(_.size == 5))
    println("NODES WITH 6 CONNECTION - " + c.count(_.size == 6))
     println("NODES WITH 7 CONNECTION - " + c.count(_.size == 7))
    println("NODES WITH 8 CONNECTION - " + c.count(_.size == 8))
    println("NODES WITH 9 CONNECTION - " + c.count(_.size == 9))
    println("NODES WITH 10 CONNECTION - " + c.count(_.size == 10))*/

    /*val fr = Oracle.freeRiders.head
    val b = Oracle.allNodes filter {
      x => x.scoreList.contains(fr)
    }

    val frconnections = b count {
      x => Oracle.freeRiders.contains(x.getID)
    }

    val altconnections = b count {
      x => !Oracle.freeRiders.contains(x.getID)
    }

    println("FR connected to FR " + frconnections)
    
    println("FR connected to alt " + altconnections)
    */

  }

  def altruisticReliability = {

    val repeated = Oracle.allNodesExceptStreamer map (_.repeatedMessages)
    println("repeated: " + repeated.sum)
    
    try {
      val hopinha = Oracle.allNodesExceptStreamer map (_.avgHops)
      val altHops = (Oracle.altruistics map {
        id => Oracle.getNode(id)
      }).map(_.avgHops)
      println(" ALT above 5.3 -> " + altHops.count(_ > 5.3))
      println(" ALT hopinha max " + altHops.max)
      println(" ALT hopinha min " + altHops.min)
      println("above 5.3 -> " + hopinha.count(_ > 5.3))
      println("hopinha max " + hopinha.max)
      println("hopinha min " + hopinha.min)

      val a = for (a <- 1000 until 1100) yield Oracle.getNode(a).messageList.filter(_ > 9000).size.toFloat / 1000

      println("NEW " + a.sum.toFloat / a.size)

    } catch {
      case x =>
    }

    if (Oracle.avgHops.exists(_ < 0)) {
      println("NEEEEEEEEEEEEEEEEG")
    }
    if (!Oracle.avgHops.isEmpty) {
      val b: Float = Oracle.avgHops.sum.toFloat
      val size: Long = Oracle.avgHops.size
      println("size : " + size)
      println("sum of avgs " + b)
      println("AVG HOPSaS " + b / size)
    }

    println("MAX HOPS " + Oracle.maxHopInfo)
    println("puzzles AFTER FREERIDERS: " + Oracle.altruisticChallanges)

    val percentages = Oracle.altruistics map {
      id =>
        //    (Oracle.getNode(id).messageList.count(_.isDefined)).toFloat / BasicGossip.cycles
        Oracle.getNode(id).messageList.size.toFloat / BasicGossip.cycles
    }

    val last1 = Oracle.altruistics map {
      id =>
        //    (Oracle.getNode(id).messageList.count(_.isDefined)).toFloat / BasicGossip.cycles
        Oracle.getNode(id).messageList.filter(_ > BasicGossip.cycles - 1000).size.toFloat / 1000
    }

    val last = Oracle.altruistics map {
      id => Oracle.getNode(id).messageList.filter(_ >= BasicGossip.cycles - 500).size.toFloat / 500
    }

    println("false positives : " + Oracle.badKicked.size)
    println("false positives 2: " + Oracle.badKicked.filter(_._2 > 1).size)
    println("num of ninja riders " + (Oracle.frAmount - Oracle.kicked.size))

    println("Reliability1: " + percentages.sum / percentages.size)

    //    println("Reliability of altruistics (0 - 10%): " + percentages.filter(x => x >= 0 && x < 0.1).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of altruistics (10 - 20%): " + percentages.filter(x => x >= 0.1 && x < 0.2).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of altruistics (20 - 30%): " + percentages.filter(x => x >= 0.2 && x < 0.3).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of altruistics (30 - 40%): " + percentages.filter(x => x >= 0.3 && x < 0.4).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of altruistics (40 - 50%): " + percentages.filter(x => x >= 0.4 && x < 0.5).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of altruistics (50 - 60%): " + percentages.filter(x => x >= 0.5 && x < 0.6).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of altruistics (60 - 70%): " + percentages.filter(x => x >= 0.6 && x < 0.7).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of altruistics (70 - 80%): " + percentages.filter(x => x >= 0.7 && x < 0.8).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of altruistics (80 - 90%): " + percentages.filter(x => x >= 0.8 && x < 0.9).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of altruistics (90 - 100%): " + percentages.filter(x => x >= 0.9 && x < 1).size.toFloat / Oracle.altruistics.size)

    println("Reliability of altruistics (>0.80%): " + percentages.filter(_ > 0.8).size.toFloat / Oracle.altruistics.size)
    println("Reliability of altruistics (>0.90%): " + percentages.filter(_ > 0.9).size.toFloat / Oracle.altruistics.size)
    println("Reliability of altruistics (>0.95%): " + percentages.filter(_ > 0.95).size.toFloat / Oracle.altruistics.size)
    println("Reliability of altruistics (>0.98%): " + percentages.filter(_ > 0.98).size.toFloat / Oracle.altruistics.size)

    //    println("Reliability of altruistics (0 - 10%) LAST: " + last.filter(x => x >= 0 && x < 0.1).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of altruistics (10 - 20%) LAST: " + last.filter(x => x >= 0.1 && x < 0.2).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of altruistics (20 - 30%) LAST: " + last.filter(x => x >= 0.2 && x < 0.3).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of altruistics (30 - 40%) LAST: " + last.filter(x => x >= 0.3 && x < 0.4).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of altruistics (40 - 50%) LAST: " + last.filter(x => x >= 0.4 && x < 0.5).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of altruistics (50 - 60%) LAST: " + last.filter(x => x >= 0.5 && x < 0.6).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of altruistics (60 - 70%) LAST: " + last.filter(x => x >= 0.6 && x < 0.7).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of altruistics (70 - 80%) LAST: " + last.filter(x => x >= 0.7 && x < 0.8).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of altruistics (80 - 90%) LAST: " + last.filter(x => x >= 0.8 && x < 0.9).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of altruistics (90 - 100%) LAST: " + last.filter(x => x >= 0.9 && x <= 1).size.toFloat / Oracle.altruistics.size)

    println("Reliability of altruistics (>0.90%): (last 500 rounds)" + last.filter(_ > 0.9).size.toFloat / Oracle.altruistics.size)
    println("Reliability of altruistics (>0.95%): (last 500 rounds)" + last.filter(_ > 0.95).size.toFloat / Oracle.altruistics.size)
    println("Reliability of altruistics (>0.98%): (last 500 rounds)" + last.filter(_ > 0.98).size.toFloat / Oracle.altruistics.size)

    println("messages # " + Oracle.altruisticsAmountOfSentMessages)
  }

  def freeriderReliability = {
    println("    num of ninja riders " + (Oracle.frAmount - Oracle.kicked.size))

    val percentages = Oracle.freeRiders map {
      x => Oracle.getNode(x).messageList.size.toFloat / BasicGossip.cycles
    }

    //    println("Reliability of free riders (0 - 10%): " + percentages.filter(x => x >= 0 && x < 0.1).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of free riders (10 - 20%): " + percentages.filter(x => x >= 0.1 && x < 0.2).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of free riders (20 - 30%): " + percentages.filter(x => x >= 0.2 && x < 0.3).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of free riders (30 - 40%): " + percentages.filter(x => x >= 0.3 && x < 0.4).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of free riders (40 - 50%): " + percentages.filter(x => x >= 0.4 && x < 0.5).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of free riders (50 - 60%): " + percentages.filter(x => x >= 0.5 && x < 0.6).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of free riders (60 - 70%): " + percentages.filter(x => x >= 0.6 && x < 0.7).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of free riders (70 - 80%): " + percentages.filter(x => x >= 0.7 && x < 0.8).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of free riders (80 - 90%): " + percentages.filter(x => x >= 0.8 && x < 0.9).size.toFloat / Oracle.altruistics.size)
    //    println("Reliability of free riders (90 - 100%): " + percentages.filter(x => x >= 0.9 && x <= 1).size.toFloat / Oracle.altruistics.size)

    /*
    val FRpercentages = Oracle.freeRiders map {
      x => Oracle.getNode(x).messageList.size.toFloat / BasicGossip.cycles
    }

    println("FRpercentages: " + percentages.sum / percentages.size)

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
    println("Reliability of Free Riders above 60%: " + percentages.filter(_ > 0.60).size.toFloat / Oracle.frAmount)
    println("Reliability of Free Riders above 70%: " + percentages.filter(_ > 0.70).size.toFloat / Oracle.frAmount)
    println("Reliability of Free Riders above 80%: " + percentages.filter(_ > 0.80).size.toFloat / Oracle.frAmount)

    val abovescore = percentages filter (_ > 0.9)
    println("Reliability of Free Riders above 90%: " + abovescore.size.toFloat / Oracle.frAmount)
    println("Reliability of Free Riders above 95%: " + percentages.filter(_ > 0.95).size.toFloat / Oracle.frAmount)
    println("Reliability of Free Riders above 98%: " + percentages.filter(_ > 0.98).size.toFloat / Oracle.frAmount)
*/

    val last = Oracle.freeRiders map {
      id => Oracle.getNode(id).messageList.filter(_ >= BasicGossip.cycles - 500).size.toFloat / 500
    }
    println("GLBAL FR" + last.sum.toFloat / last.size)
    println("Reliability of FR (>0.90%): (last 500 rounds)" + last.filter(_ > 0.9).size.toFloat / Oracle.freeRiders.size)
    println("Reliability of FR (>0.95%): (last 500 rounds)" + last.filter(_ > 0.95).size.toFloat / Oracle.freeRiders.size)
    println("Reliability of FR (>0.98%): (last 500 rounds)" + last.filter(_ > 0.98).size.toFloat / Oracle.freeRiders.size)

    println("Amounf of messages: " + Oracle.altruisticsAmountOfSentMessages + Oracle.frAmountOfSentMessages)

    val altruistics = (1 - Oracle.frPercentage) * Network.size
    println("Avg Altruistic Message #: " + Oracle.altruisticsAmountOfSentMessages / altruistics)

    if (Oracle.frAmount != 0) {
      println("Avg FR Message #: " + Oracle.frAmountOfSentMessages / Oracle.frAmount)
    }

    println("ALT PUZZLES " + Oracle.altruisticChallanges)

    println("FR PUZZLES " + Oracle.FRchallenges)

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
          x => print(x + " "))
        }
        println
      case _ =>
    }
  }*/

}

object AvgReliability extends AvgReliability("mamanavara")