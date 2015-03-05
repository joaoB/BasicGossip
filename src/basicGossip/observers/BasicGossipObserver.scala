package basicGossip.observers

import basicGossip.oracle.Oracle
import basicGossip.protocols.BasicGossip
import peersim.core.Control
import peersim.core.Network
import basicGossip.node.Usernode

class BasicGossipObserver(name: String) extends Control {

  def execute = {
    for (i <- 0 until Network.size) {
      Network.get(i) match {
        case node: Usernode => //println(node)// println(node.messageList)
        //case node: Usernode => node.dumpAmoutOfMessage
        case _ => ???
      }
    }
    dumpMinAndMaxPercentages
    dumpMaxHops
    dumpTotalMessages
    dumpAvgReliability
    println("fanout " + BasicGossip.fanout)
    //checkSimmetryScores
    //dumpFreeRiders

    //      print("Max Hop Info -> ")
    //      println(Oracle.maxHopInfo.hop)     

    /*println("NODE 42 scorelist")
    Network.get(42) match { case a: Usernode => a.scoreList map(x => println (x._1 + " -> " + x._2)) }
*/
    //TODO: meter "<90" a ir buscar o 90 ao file.. o score de Free Riders tambem
    /*for (i <- 0 until Network.size()) {
      Network.get(i) match {
        case node: Usernode => 
          println("NODE: " + i + " detected -> " + node.scoreList.filter(x => x._2 < -2 && x._1 < 90 && x._1 > 0).size + " positive free riders")
          println("NODE: " + i + " detected -> " + node.scoreList.filter(x => x._2 < -2 && x._1 >= 90).size + " false free riders")
        case _ => ???
      }
    }*/

    false
  }

  private def dumpMinAndMaxPercentages = {
    val percentages = (for (i <- 1 until Network.size) yield Network.get(i)) map {
      elem =>
        elem match {
          case un: Usernode => un.messageList.size.toFloat / BasicGossip.cycles
        }
    }
    println("Min percentage: " + percentages.min)
    println("Max percentage: " + percentages.max)
    println("Amount of nodes with 100% -> " + percentages.count { x => x == 1.0 })
  }

  private def dumpMaxHops = {
    Oracle.maxHopInfo match {
      case Some(elem) => println("Max hops -> " + elem.hop)
      case None => //should never happen
    }
  }

  private def dumpTotalMessages = {
    println("Amounf of messages: " + Oracle.amountOfSentMessages)
  }

  private def dumpAvgReliability = {
    val percentages = (for (i <- 1 until Network.size) yield Network.get(i)) map {
      elem =>
        elem match {
          case un: Usernode => un.messageList.size.toFloat / BasicGossip.cycles
        }
    }
    println("Reliability: " + percentages.sum / percentages.size)
  }

  private def dumpFreeRiders = {
    for (i <- 1 until Network.size) {
      Network.get(i) match {
        case us: Usernode =>
          us.dumpFreeRiders
      }
    }
  }

  private def test2 = {
    //the node with less % is also the node with less connections?
    // YES!!!
    for (i <- 1 until Network.size) {
      print("Node: " + i + " ->")
      Network.get(i) match {
        case un: Usernode => println(un.scoreList.size + " " + un.messageList.size.toFloat / BasicGossip.cycles)
      }
    }
  }

  private def test = {
    Network.get(0) match {
      case streamer: Usernode => println(streamer)
    }

    for (i <- 1 until Network.size) {
      print("Node: " + i + " ->")
      Network.get(i) match {
        case un: Usernode => println(un.scoreList.filter(_._2 > 0))
      }
    }
  }

  private def test3 = {
    //does any node has a connection to itself????
    //guess no...
    for (i <- 0 until Network.size) {
      print("Node: " + i + " ->")
      Network.get(i) match {
        case un: Usernode => un.scoreList.find(_._1 == un.getID) match {
          case Some(elem) => println("BINGO!!!! PIMBA NE BELHA")
          case None => println
        }
      }
    }
  }

  private def checkSimmetryScores = {
    for (i <- 0 until Network.size) {
      print("Node: " + i + " ->")
      Network.get(i) match {
        case un: Usernode => println(un.scoreList)
        
      }
    }
  }

}