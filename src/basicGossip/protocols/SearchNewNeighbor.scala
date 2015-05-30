package basicGossip.protocols

import peersim.cdsim.CDProtocol
import peersim.core.Node
import basicGossip.node.Usernode
import basicGossip.oracle.Oracle
import peersim.core.Network
import peersim.edsim.EDProtocol
import scala.util.Random
import hyparview.MyHyParView

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
    lookForNeighbors(un)

  }

  def lookForNeighbors(un: Usernode) = {
    if (un.scoreList.size < Oracle.MIN_WIN_TO_SEARCH) {
      Random.shuffle(un.scoreList).headOption match {
        case Some(elem) =>
          val node = Oracle.getNode(elem._1.toInt)
          MyHyParView.join(node, un)
        case None =>
      }
    }
  }

  def kickFreeRiders(un: Usernode) = {

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

          Oracle.getLinkable(fr.toInt).removeNeighbor(un)
          link.removeNeighbor(Oracle.getNode(fr.toInt))
      }
    }
  }

}