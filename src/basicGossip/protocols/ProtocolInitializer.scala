package basicGossip.protocols

import basicGossip.node.Usernode
import basicGossip.oracle.Oracle
import peersim.config.Configuration
import peersim.core.Control
import peersim.core.IdleProtocol
import peersim.core.Network
import peersim.core.Node
import peersim.dynamics.NodeInitializer
import scala.util.Random
import basicGossip.observers.AvgReliability

class ProtocolInitializer(name: String) extends Control with NodeInitializer {

  val frPercentage = Oracle.frPercentage
  val protocolID = Configuration.getInt(name + "." + "PROTOCOL_ID")
  val streamerID = 0

  def execute = {
  
    /*Oracle.altruistics map {
      id => initializeAltruistic(Oracle.getNode(id))
    }

    Oracle.freeRiders map {
      id => initializeFreeRider(Oracle.getNode(id))
    }

    initializeViews*/
    false
  }

  def initializeAltruistic(node: Usernode) {
    protocolID match {
      case 1 => node.setProtocol(0, new AltruisticProtocol("Altruistic Protocol"))
      case 2 => node.setProtocol(0, new AltruisticWithMaxHops("basicGossip.protocols.AltruisticWithMaxHops"))
      case 3 => node.setProtocol(0, new AltruisticThirdModel("basicGossip.protocols.AltruisticThirdModel"))
      case 4 => node.setProtocol(0, new ThreeFaseGossip("basicGossip.protocols.ThreeFaseGossip"))
      case _ => ???
    }

  }

  def initializeFreeRider(node: Usernode) {
    //node.setProtocol(0, new RationalProtocol("Free Rider Protocol"))
    node.setProtocol(0, new FRProtocol("Free Rider Protocol"))
  }

  def initialize(node: Node) = {}

  def initializeViews = {
    //hyparview initialization

   /* Oracle.nodesHpvProtocol map {
      case (un: Usernode, prot: HyParViewJoinTest) =>
        prot.setMyNode(Network.get(un.getID.toInt), Oracle.getViewSize(un))
    }

    val streamerHpv = Oracle.nodeHpvProtocol(streamerID)

    Oracle.allNodesExceptStreamer map {
      node => streamerHpv._2.join(Network.get(node.getID.toInt), HyParViewJoinTest.protocolID)
    }*/
   // rejoinIsolated()

    //    val maxN = 10
    //    for (id <- 1 to maxN) {
    //          Oracle.nodesHpvProtocol.filter(Oracle.freeRiders contains _._1.getID) map {
    //            x => x._2.simpleJoin(x._1, HyParViewJoinTest.protocolID)
    //          }
    //
    //    }
    //    
    //    val maxN = Oracle.RACIONAL_MAX_CONNECTIONS
    //    Oracle.nodesHpvProtocol.filter(Oracle.freeRiders contains _._1.getID).filter(_._2.neighbors.size > maxN) map {
    //      case (un: Usernode, prot: HyParViewJoinTest) =>
    //        prot.neighbors.take(prot.neighbors.size - maxN) map {
    //          x =>
    //            prot.setMyNode(Network.get(un.getID.toInt), Oracle.getViewSize(un))
    //            prot.disconnect(x)
    //
    //            Oracle.nodeHpvProtocol(x.getID.toInt)._2.disconnect(un)
    //  
    //        }
    //
    //    }

//    val maxView = 10
//    val un = Oracle.nodesHpvProtocol.filter(x => Oracle.freeRiders.contains(x._1.getID))
//    un.filter(_._2.neighbors.size > maxView) map {
//      x =>
//        while (x._2.neighbors.size > maxView) {
//          Oracle.nodesHpvProtocol(x._2.neighbors.head.getID.toInt)._2.disconnect(x._1)
//          x._2.disconnect(x._2.neighbors.head)
//        }
//    }

    //        while(un._2.neighbors.size < maxN ){
    //         Oracle.nodeHpvProtocol(Random.nextInt(1000))._2.simpleJoin(Network.get(un._1.getID.toInt), HyParViewJoinTest.protocolID)
    //          Oracle.FRchallenges += 1
    //        }

    //println(Oracle.nodesHpvProtocol.filter(Oracle.freeRiders contains _._1.getID).head._2.neighbors().size)

    //globalHyParViewLinkage
    
    //traditionalHyParViewLinkage

    /*    Oracle.nodeHpvProtocol(Oracle.getLinkable(2).getNeighbors.head.getID.toInt)._2.simpleJoin(Oracle.getNode(2), HyParViewJoinTest.protocolID)

    Oracle.allNodesExceptStreamer map {
      elem => Oracle.nodeHpvProtocol(Oracle.getLinkable(elem.getID.toInt).getNeighbors.head.getID.toInt)._2.simpleJoin(Oracle.getNode(elem.getID.toInt), HyParViewJoinTest.protocolID)

    }*/

    /*Oracle.nodesHpvProtocol /*filter (_._1.getID == 2) */ map {
      elem =>
        print("NODE: " + elem._1.getID + " -> ")
        elem._2.neighbors map {
          x => print(x.getID + " ")
        }
        println
    }*/
  }

//  private def rejoinIsolated(solve: Boolean = false) = {
//    def rejoinIsolatedAux: Boolean = {
//      val streamerHpv = Oracle.nodeHpvProtocol(streamerID)
//      val a = Oracle.nodesHpvProtocolExceptStreamer.filter(x => x._2.neighbors.size < Oracle.minWindow) map {
//        a =>
//          streamerHpv._2.simpleJoin(Network.get(a._1.getID.toInt), HyParViewJoinTest.protocolID)
//      }
//      Oracle.nodesHpvProtocolExceptStreamer.exists(x => x._2.neighbors.size < Oracle.minWindow)
//    }
//    while (rejoinIsolatedAux) {}
//  }
/*
  private def globalHyParViewLinkage = {
    //streamer knows everybody
    val streamer = Oracle.getNode(streamerID)
    val streamerLink = Oracle.getLinkable(streamer)
    Oracle.nodesHpvProtocolExceptStreamer map {
      case (un: Usernode, prot: HyParViewJoinTest) =>
        un.initializeScoreList(prot.neighbors.toSeq map (x => x.getID))
        prot.neighbors map {
          neigh =>
            val unLink = Oracle.getLinkable(un)
            unLink.addNeighbor(Network.get(streamerID))
            unLink.addNeighbor(neigh)
            streamerLink.addNeighbor(un)
        }
    }
/*
    AvgReliability.c

    println("---------------------------------------------------------------------")

    for (id <- 0 until 100) {
      Oracle.addAltruisticNode
    }
    AvgReliability.c
    println("SIMPLE JOINS " + Oracle.simpleJoins)
    val a = (for (id <- 1000 until 1100) yield Oracle.getNode(id).scoreList.size) toList
      
    println("SCORE LIST " + Oracle.getNode(1000).scoreList.keySet)
    println("HPV " + Oracle.nodeHpvProtocol(1000)._2.neighbors().map(_.getID).toList)
    println("MIN size  " + a.min)
    println("MAX size " + a.max)
    println("avg new comers score size " + a.sum / a.size)
*/
    /*
    for (idRemove <- 1 to 300) {
      Oracle.getNode(idRemove).setFailState(1)

    }

    for (id <- 301 to 999) {
      for (idRemove <- 1 to 300) {
        Oracle.getNode(id).removeFromScoreList(idRemove)
        Oracle.nodeHpvProtocol(id)._2.disconnect(Network.get(idRemove))
      }
    }
    var a = 0
    for (id <- 301 to 999) {

      var count = 0
      

      while (count < 90000 && Oracle.nodeHpvProtocol(id)._2.neighbors.size < Oracle.minWindow) {
        count += 1

        val c = Oracle.nodeHpvProtocol(id)._2.neighbors.size

        Oracle.nodeHpvProtocol(0)._2.simpleJoin(Network.get(id), HyParViewJoinTest.protocolID)
        val b = Oracle.nodeHpvProtocol(id)._2.neighbors.size

        if (c != b) {
          a += 1
        }

      }

    }
    val ww= 301 to 999 map(x => Oracle.getNode(x)) toList
    val laura = Oracle.nodesHpvProtocol(ww) map {
      x => x._2.neighbors().size
    }
    
    println("larua min " + laura.min)
    
    println("A-> " + a)*/
  }

  private def traditionalHyParViewLinkage = {
    Oracle.nodesHpvProtocol map {
      case (un: Usernode, prot: HyParViewJoinTest) =>
        un.initializeScoreList(prot.neighbors.toSeq map (x => x.getID))
        prot.neighbors map {
          neigh => Oracle.getLinkable(un).addNeighbor(neigh)
        }
    }
  }
*/
}