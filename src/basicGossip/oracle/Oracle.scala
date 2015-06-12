package basicGossip.oracle

import scala.collection.mutable.MutableList
import scala.util.Random
import basicGossip.messages.Info
import basicGossip.node.Usernode
import basicGossip.protocols.AltruisticProtocol
import basicGossip.protocols.Link
import peersim.config.Configuration
import peersim.config.FastConfig
import peersim.core.Network
import hyparview.MyHyParView
import hyparview.MyHyParView
import scala.None
import basicGossip.node.Usernode
import basicGossip.protocols.FRProtocol

//Oracle has an eye on everythinggi
class Oracle extends AddNode {

  val maxHops = Configuration.getInt("Oracle.MAX_HOPS")
  val peerAlgorithm = Configuration.getInt("Oracle." + "PEER_ALGORITHM")
  val fanout = Configuration.getInt("Oracle.FANOUT")
  val redundancyFactor = Configuration.getDouble("Oracle.REDUNDANCY_FACTOR")
  val baseRank = Configuration.getDouble("Oracle.BASE_RANK")
  val forwardProbability = Configuration.getDouble("Oracle.FORWARD_PROBABILITY")
  val FR_THRESHOLD = Configuration.getInt("Oracle.FR_THRESHOLD")
  val MIN_WIN_TO_SEARCH = Configuration.getInt("Oracle.MIN_WINDOW_TO_SEARCH")
  val QUARANTINE = Configuration.getInt("Oracle.QUARANTINE")

  val MAX_WIN = Configuration.getInt("Oracle.MAX_WIN")
  val RACIONAL_MAX_CONNECTIONS = Configuration.getInt("Oracle.RACIONAL_MAX_CONNECTIONS")

  var kicked = Map[Int, Int]()
  var badKicked = Map[Int, Int]()
  var maxHopInfo = 0
  var altruisticsAmountOfSentMessages: Int = 0
  var frAmountOfSentMessages: Int = 0
  var avgHops = MutableList[Int]()

  var challengesBeforeStream = 0
  var FRchallenges = 0
  var altruisticChallanges = 0
  var currentPackage = 0
  var disconnectsBeforeStream = 0
  var disconnects = 0
  def incChallengesBeforeStream = challengesBeforeStream += 1
  def incDisconnects = disconnectsBeforeStream += 1

  def updateCurrentPackage = currentPackage += 1

  def kick(id: Int) = {
    if (altruistics.contains(id)) {
      badKick(id)
    } else {
      kicka(id)
    }

  }

  private def kicka(id: Int) = {
    kicked = kicked match {
      case map if map.contains(id) =>
        kicked.updated(id, kicked(id) + 1)
      case _ => kicked.updated(id, 1)
    }
  }

  private def badKick(id: Int) =
    badKicked = badKicked match {
      case map if map.contains(id) =>
        badKicked.updated(id, badKicked(id) + 1)
      case _ => badKicked.updated(id, 1)
    }

  def saveMaxHopInfo(info: Info) =
    if (info.hop > maxHopInfo && currentPackage > 5000) {
      maxHopInfo = info.hop
    }

  def incChallenges(un: Usernode) =
    if (currentPackage > 2499) {
      if (!Oracle.freeRiders.contains(un.getID)) {
        altruisticChallanges += 1
      } else {
        FRchallenges += 1
      }
    }

  def incSentMessages(un: Usernode) =
    if (currentPackage > 5000)
      if (!Oracle.freeRiders.contains(un.getID)) {
        altruisticsAmountOfSentMessages += 1
      } else {
        frAmountOfSentMessages += 1
      }

  private def updateMaxHopInfo(info: Info) =
    maxHopInfo = info.value

  def saveHop(info: Info) = {
    if (currentPackage > 5000)
      avgHops.+=(info.hop)
  }

  def getLinkable(usernode: Usernode): Link =
    usernode.getProtocol(FastConfig.getLinkable(0)) match {
      case link: Link => link
    }

  def getLinkable(nid: Int): Link = getLinkable(getNode(nid))

  def getNode(id: Int): Usernode = {
    Network.get(id) match {
      case un: Usernode => un
    }
  }

}

object Oracle extends Oracle