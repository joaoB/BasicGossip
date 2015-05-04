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

class AltruisticProtocol(name: String) extends GeneralProtocol {

  override def shouldLookForNewNeighbor(un: Usernode): Boolean = {
    Oracle.nodeHpvProtocol(un.getID.toInt)._2.neighbors.size < Oracle.minWindow && un.solvingChallenges.size == 0
  }

  override def computeFanout(gossiper: Usernode, sender: Node): Set[Long] =
    Oracle.peerAlgorithm match {
      case 0 =>
        val probability = Random.nextFloat
        gossiper.scoreList.filter(x => x._1 != 0 && x._1 != sender.getID && x._2 > probability).keySet
      case 1 =>
        gossiper.scoreList.filter(x => x._1 != 0).keySet
    }

  override def initializeScoreList(un: Usernode, ids: Seq[Long]) = {
    un.scoreList = Map(ids map {
      id => id -> 1F
    }: _*)
  }

  override def canAcceptNewNeighbor(un: Usernode) = Oracle.getLinkable(un).neigh.size < Oracle.fanout

  override def addToScoreList(un: Usernode, nid: Long) {
    un.scoreList = un.scoreList.updated(nid, 0.5F)
  }
}