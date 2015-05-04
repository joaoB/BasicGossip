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
    un.scoreList = un.scoreList.updated(nid, 0.0F)
  }

  override def canAcceptNewNeighbor(un: Usernode) = true

  override def computeFanout(gossiper: Usernode, sender: Node): Set[Long] = {
    gossiper.scoreList.filter(x => x._1 != 0 && x._2 > 1 - (gossiper.FR_THRESHOLD + 0.6)).keySet
  }
  override def shouldLookForNewNeighbor(un: Usernode): Boolean = {
    true
  }
}