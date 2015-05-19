package basicGossip.protocols

import basicGossip.messages.Info
import basicGossip.node.Usernode
import basicGossip.oracle.Oracle
import peersim.transport.Transport
import peersim.config.FastConfig
import peersim.core.Node
import scala.util.Random

class RationalProtocol(name: String) extends GeneralProtocol {

  def initializeScoreList(un: Usernode, ids: Seq[Long]) = {
    un.scoreList = Map(ids map {
      id => id -> 0F
    }: _*)
  }

  override def addToScoreList(un: Usernode, nid: Long) {
    un.scoreList = un.scoreList.updated(nid, Oracle.baseRank.toFloat)
  }

  override def canAcceptNewNeighbor(un: Usernode) = {
    val max = 10
    Oracle.nodeHpvProtocol(un.getID.toInt)._2.neighbors.size < max &&
      un.waitingConfirm.size + un.scoreList.size + un.solvingChallenges.size < max

  }

  override def computeFanout(gossiper: Usernode, sender: Node): Set[Long] = {
    val neighbors = gossiper.scoreList.filter(x => x._1 != 0 && x._1 != sender.getID) filter {
      //     case (id, score) if score >= 13 => true
      //   case _ => false

      case (id, score) if score >= Oracle.baseRank => aboveBaseRank
      case (id, score) if Oracle.baseRank > score && score >= Oracle.FR_THRESHOLD => belowBaseRank(score)
      case (id, score) if score < Oracle.FR_THRESHOLD => false

    }
    //println(neighbors.keySet.size)
    neighbors.keySet
  }
  override def shouldLookForNewNeighbor(un: Usernode): Boolean = {
    canAcceptNewNeighbor(un)
  }
}