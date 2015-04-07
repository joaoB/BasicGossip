package basicGossip.protocols

import peersim.cdsim.CDProtocol
import peersim.core.Node
import basicGossip.node.Usernode
import hyparview.HyParViewJoinTest
import basicGossip.oracle.Oracle
import peersim.core.Network
import peersim.edsim.EDProtocol

class SearchNewNeighbor(name: String) extends CDProtocol {

  def nextCycle(node: Node, pid: Int) {
    
    node match {
      case un: Usernode => execute(un)
    }
  }


  def execute(un: Usernode) {
    if (un.shouldLookForNewNeighbor /*&& un.solvingChallenges.size < 2*/ ) {
      val frs = un.freeRiders
      //      println("NODE: " + Oracle.freeRiders.contains(un.getID))

      val link = Oracle.getLinkable(un)
      if(un.getID == 1000) println("teteteetete")
      val neighID = link.neigh.headOption match {
        case Some(elem) => elem.getID.toInt
        case None => 0
      } 
      frs map {
        fr =>
          // println("NODE: " + un.getID + " will kick: " + fr + " " + Oracle.freeRiders.contains(fr))

          if (Oracle.freeRiders.contains(fr))
            Oracle.kick(fr.toInt)
          else
            Oracle.badKick(fr.toInt)

          un.removeFromScoreList(fr)
          Oracle.getNode(fr.toInt).removeFromScoreList(un.getID)

          Oracle.getLinkable(fr.toInt).removeNeighbor(un)
          link.removeNeighbor(Oracle.getNode(fr.toInt))

          Oracle.nodeHpvProtocol(un.getID.toInt)._2.disconnect(Network.get(fr.toInt))
      }

      Oracle.nodeHpvProtocol(neighID)._2.setMyNode(Network.get(neighID))
      Oracle.nodeHpvProtocol(neighID)._2.simpleJoin(un, HyParViewJoinTest.protocolID)

      (Oracle.nodeHpvProtocol(un.getID.toInt)._2.neighbors.toList diff link.neigh) map {
        newMember =>
          // Oracle.getNode(newMember.getID.toInt).addChallenge(un)
          un.addChallenge(Oracle.getNode(newMember.getID.toInt))
          if (Oracle.freeRiders contains un.getID) Oracle.FRchallenges += 1 else Oracle.altruisticChallanges += 1
          Oracle.getNode(newMember.getID.toInt).addWaitingConfirm(un.getID.toInt)
        //println("Node: " + un.getID + " has new neighbor " + newMember.getID)

        //Oracle.getLinkable(newMember.getID.toInt).addNeighbor(un)
        //link.addNeighbor(newMember)

        //Oracle.getNode(newMember.getID.toInt).addToScoreList(un.getID)
        //un.addToScoreList(newMember.getID)
      }

      //      Oracle.nodeHpvProtocol(un.getID.toInt)._2.neighbors() map {
      //        x => print(x.getID + " ")
      //      }
      //      println
    }
  }

}

object SearchNewNeighbor extends SearchNewNeighbor("mamanavara")