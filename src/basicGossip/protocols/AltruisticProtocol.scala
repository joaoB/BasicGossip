package basicGossip.protocols

import basicGossip.messages.Info
import basicGossip.node.Usernode
import peersim.config.FastConfig
import peersim.core.CommonState
import peersim.core.Linkable
import peersim.transport.Transport
import scala.util.Random
import basicGossip.oracle.Oracle
import peersim.core.Node
import hyparview.HyParViewJoinTest

class AltruisticProtocol(name: String) extends GeneralProtocol {

  override def shouldLookForNewNeighbor(un: Usernode): Boolean = {
    Oracle.nodeHpvProtocol(un.getID.toInt)._2.neighbors.size <= Oracle.MIN_WIN_TO_SEARCH && un.solvingChallenges.size == 0
  }

  override def computeFanout(gossiper: Usernode, sender: Node): Set[Long] = {

    val neighbors = gossiper.scoreList.filter(x => x._1 != 0 && x._1 != sender.getID) filter {
      //case _ => aboveBaseRank
      case (id, score) if score >= Oracle.baseRank => aboveBaseRank
      case (id, score) if Oracle.baseRank > score && score >= Oracle.FR_THRESHOLD => belowBaseRank(score)
      case (id, score) if score < Oracle.FR_THRESHOLD => false
    }

//    if (!neighbors.keySet.contains(Oracle.freeRiders.head) && gossiper.scoreList.keySet.contains(Oracle.freeRiders.head)) {
//      println("sending to calinas")
//    }

    neighbors.keySet
  }

  override def initializeScoreList(un: Usernode, ids: Seq[Long]) = {
    un.scoreList = Map(ids map {
      id => id -> Oracle.baseRank.toFloat
    }: _*)
  }

  override def canAcceptNewNeighbor(un: Usernode) =
    Oracle.nodeHpvProtocol(un.getID.toInt)._2.neighbors.size <= HyParViewJoinTest.activeViewSize

  override def addToScoreList(un: Usernode, nid: Long) {
    un.scoreList = un.scoreList.updated(nid, Oracle.baseRank.toFloat)
  }
}