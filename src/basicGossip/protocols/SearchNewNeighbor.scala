package basicGossip.protocols

import peersim.cdsim.CDProtocol
import peersim.core.Node
import basicGossip.node.Usernode
import hyparview.HyParViewJoinTest
import basicGossip.oracle.Oracle
import peersim.core.Network
import peersim.edsim.EDProtocol
import scala.util.Random

class SearchNewNeighbor(name: String) extends CDProtocol {

  def nextCycle(node: Node, pid: Int) {

    node match {
      case un: Usernode if un.getID != 0 => execute(un)
      case _ =>
    }
  }

  def execute(un: Usernode) {

    val link = Oracle.getLinkable(un)
    val neighID = Random.shuffle(link.neigh).headOption match {
      case Some(elem) => elem.getID.toInt
      case None => 0
    }

    if (un.shouldLookForNewNeighbor || kickFreeRiders(un)) {

      Oracle.nodeHpvProtocol(neighID)._2.setMyNode(Network.get(neighID), Oracle.getViewSize(Oracle.getNode(neighID)))
      //println(Oracle.getViewSize(Oracle.getNode(neighID)))
      Oracle.nodeHpvProtocol(neighID)._2.simpleJoin(un, HyParViewJoinTest.protocolID)

      val challenges = un.solvingChallenges map {
        x => x.sender.getID
      }

      val a = (Oracle.nodeHpvProtocol(un.getID.toInt)._2.neighbors.toList.map(_.getID).groupBy(identity).collect { case (x, ys) if ys.size > 1 => x })

//      Oracle.nodesHpvProtocol map {
//        x =>
//          print("Node -> " + x._1.getID + Oracle.freeRiders.contains(x._1.getID) + " : ")
//          x._2.neighbors() map {
//            y => print(y.getID + " ")
//          }
//          println
//      }
//      println
//
//      println("########################################################################")

      (Oracle.nodeHpvProtocol(un.getID.toInt)._2.neighbors.toList.map(_.getID) diff link.neigh.map(_.getID) diff challenges) map {
        newMember =>
          if (un.addChallenge(Oracle.getNode(newMember.toInt))) {
            if (Oracle.freeRiders contains un.getID) {
              //println(un.solvingChallenges.size)
              Oracle.FRchallenges += 1
            } else {
              Oracle.altruisticChallanges += 1
            }

            Oracle.getNode(newMember.toInt).addWaitingConfirm(un.getID.toInt)
          }

      }

      //      if (un.getID == Oracle.freeRiders.head) {
      //        println(Oracle.nodeHpvProtocol(un.getID.toInt)._2.neighbors.size)
      //        un.scoreList map {
      //          x => print(Oracle.getNode(x._1.toInt).getID + " ")
      //        }
      //        println
      //      }

    }
  }

  def kickFreeRiders(un: Usernode): Boolean = {
    val frs = un.freeRiders
    val link = Oracle.getLinkable(un)
    if (un.freeRiders.size > 0) {
      
//      if (un.freeRiders contains Oracle.freeRiders.head) {
//        println("Node: " + un.getID + " will kick free rider")
//        val a = un.scoreList(Oracle.freeRiders.head)
//        println ("score: " + a)
//        println("FR -> kicker " + Oracle.getNode(Oracle.freeRiders.head).scoreList(un.getIndex))
//      }
      
      
      frs map {
        fr =>
          if (Oracle.freeRiders.contains(fr))
            Oracle.kick(fr.toInt)
          else
            Oracle.badKick(fr.toInt)

          un.removeFromScoreList(fr)
          Oracle.getNode(fr.toInt).removeFromScoreList(un.getID)

          Oracle.getLinkable(fr.toInt).removeNeighbor(un)
          link.removeNeighbor(Oracle.getNode(fr.toInt))

          Oracle.nodeHpvProtocol(un.getID.toInt)._2.disconnect(Network.get(fr.toInt))
          Oracle.nodeHpvProtocol(fr.toInt)._2.disconnect(Network.get(un.getID.toInt))
      }
      true
    }

    false
  }

}