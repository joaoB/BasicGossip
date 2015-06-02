package basicGossip.protocols

import peersim.cdsim.CDProtocol
import peersim.core.Node
import basicGossip.node.Usernode
import basicGossip.oracle.Oracle
import peersim.core.Network
import peersim.edsim.EDProtocol
import scala.util.Random
import hyparview.MyHyParView
import basicGossip.node.NodeStatus

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

    kickFreeRiders(un)

    /*if (un.scoreList.filter(x => x._2.status != NodeStatus.SOLVING).size > Oracle.MIN_WIN_TO_SEARCH) {
      un.scoreList.filter(x => x._2.status == NodeStatus.SOLVING) map {
        node =>
          un.solvingChallenges = un.solvingChallenges.filter(_.sender.getID != node._1)
          un.removeFromScoreList(node._1)
          Oracle.getNode(node._1.toInt).removeFromScoreList(un.getID)
      }
    }*/
    
    val list = un.solvingChallenges.map(_.sender.getID).toList
    val set = un.solvingChallenges.map(_.sender.getID).toSet

    if (un.shouldLookForNewNeighbor) {
      MyHyParView.join(un)
    }
  }

  def kickFreeRiders(un: Usernode): Boolean = {

    val frs = un.freeRiders
    val link = Oracle.getLinkable(un)
    if (un.freeRiders.size > 0) {
      frs map {
        fr =>
          if (Oracle.freeRiders.contains(fr))
            Oracle.kick(fr.toInt)
          else
            Oracle.badKick(fr.toInt)

          un.removeFromScoreList(fr)
          Oracle.getNode(fr.toInt).removeFromScoreList(un.getID)
      }
      false
    }

    false
  }

}