package basicGossip.oracle

import scala.collection.mutable.MutableList
import scala.util.Random

import basicGossip.messages.Info
import basicGossip.node.Usernode
import basicGossip.protocols.AltruisticProtocol
import basicGossip.protocols.Link
import hyparview.HyParViewJoinTest
import peersim.config.Configuration
import peersim.config.FastConfig
import peersim.core.Network

//Oracle has an eye on everythinggi
class Oracle {

  val frPercentage = Configuration.getDouble("Oracle.FR_PERCENTAGE")
  val maxHops = Configuration.getInt("Oracle.MAX_HOPS")
  val peerAlgorithm = Configuration.getInt("Oracle." + "PEER_ALGORITHM")
  val fanout = Configuration.getInt("Oracle.FANOUT")
  val minWindow = Configuration.getInt("Oracle.MIN_WINDOW")
  val redundancyFactor = Configuration.getDouble("Oracle.REDUNDANCY_FACTOR")
  val baseRank = Configuration.getDouble("Oracle.INITIALIZE_SCORE")
  val forwardProbability = Configuration.getDouble("Oracle.FORWARD_PROBABILITY")
  val FR_THRESHOLD = Configuration.getInt("Oracle.FR_THRESHOLD")
  val MIN_WIN_TO_SEARCH = Configuration.getInt("Oracle.MIN_WINDOW_TO_SEARCH")
  val QUARANTINE = Configuration.getInt("Oracle.QUARANTINE")

  val RACIONAL_MAX_CONNECTIONS = Configuration.getInt("Oracle.RACIONAL_MAX_CONNECTIONS")

  val total = 1 until Network.size toList
  val frAmount = (Network.size * frPercentage).toInt
  val freeRiders = Random.shuffle(total).take(frAmount)
  var altruistics = total diff freeRiders
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

  def incChallengesBeforeStream = challengesBeforeStream += 1
  def incDisconnects = disconnectsBeforeStream += 1

  def updateCurrentPackage = currentPackage += 1

  def kick(id: Int) = {
    kicked = kicked match {
      case map if map.contains(id) =>
        kicked.updated(id, kicked(id) + 1)
      case _ => kicked.updated(id, 1)
    }
  }

  def badKick(id: Int) =
    badKicked = badKicked match {
      case map if map.contains(id) =>
        badKicked.updated(id, badKicked(id) + 1)
      case _ => badKicked.updated(id, 1)
    }

  def saveMaxHopInfo(info: Info) =
    maxHopInfo match {
      case elem if info.hop >= elem => updateMaxHopInfo(info)
      case _ => //updateMaxHopInfo(info)
    }

  def incSentMessages(un: Usernode) =
    if (!Oracle.freeRiders.contains(un.getID)) {
      altruisticsAmountOfSentMessages += 1
    } else {
      frAmountOfSentMessages += 1
    }

  private def updateMaxHopInfo(info: Info) =
    maxHopInfo = info.value

  def saveHop(info: Info) =
    avgHops.+=(info.hop)

  private def allNodesAux(start: Int): List[Usernode] =
    (for (i <- start until Network.size) yield Network.get(i)).map {
      case un: Usernode => un
    } toList

  def allNodesExceptStreamer = allNodesAux(1)

  def allNodes = allNodesAux(0)

  def nodesHpvProtocolExceptStreamer: List[(Usernode, HyParViewJoinTest)] = {
    nodesHpvProtocol(allNodesExceptStreamer)
  }

  def nodesHpvProtocol: List[(Usernode, HyParViewJoinTest)] =
    nodesHpvProtocol(Oracle.allNodes)

  def nodesHpvProtocol(nodes: List[Usernode]): List[(Usernode, HyParViewJoinTest)] = {
    nodes map {
      node =>
        node.getProtocol(HyParViewJoinTest.protocolID) match {
          case prot: HyParViewJoinTest => (node, prot)
        }
    } toList
  }

  def nodesHpvProtocols(nodes: List[Int]): List[(Usernode, HyParViewJoinTest)] = {
    nodesHpvProtocol(nodes map {
      node => Oracle.getNode(node)
    })
  }

  def nodeHpvProtocol(id: Int): (Usernode, HyParViewJoinTest) = {
    nodesHpvProtocol(List(getNode(id))).head
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

  var simpleJoins = 0
  def addAltruisticNode = {
    val node = new Usernode("usernode")
    Network.add(node)
    val nodeID = node.getID.toInt
    val nodeNode = Network.get(nodeID)

    node.setProtocol(0, new AltruisticProtocol("Altruistic Protocol"))
    nodesHpvProtocol(nodeID)._2.setMyNode(nodeNode, getViewSize(node))

    for (id <- 0 until HyParViewJoinTest.activeViewSize) {      
      simpleJoins += 1     
      val streamerHpv = Oracle.nodeHpvProtocol(Random.nextInt(Network.size))
      streamerHpv._2.simpleJoin(nodeNode, HyParViewJoinTest.protocolID, true)
    }

    val prot = Oracle.nodeHpvProtocol(node.getID.toInt)._2.neighbors
    node.initializeScoreList(prot.toSeq map (x => x.getID))
    prot map {
      x =>
        val unLink = Oracle.getLinkable(node)
        //unLink.addNeighbor(Network.get(0))
        unLink.addNeighbor(x)
        node.addChallenge(Oracle.getNode(x.getID.toInt))
        Oracle.getNode(x.getID.toInt).addWaitingConfirm(node.getID.toInt)

        Oracle.getLinkable(x.getID.toInt).addNeighbor(node)
        x match {
          case a: Usernode if a.getID != 0 => 
            a.addToScoreList(node.getID)
          case _ =>
        }

    }

    altruistics = altruistics.::(node.getID.toInt)
    
  }

  def getViewSize(un: Usernode) = {
    //    un.getID match {
    //      case 0 => Network.size
    //      case n if Oracle.freeRiders.contains(n) => RACIONAL_MAX_CONNECTIONS
    //      case n if !Oracle.freeRiders.contains(n) => HyParViewJoinTest.activeViewSize
    //    }
    if (Oracle.freeRiders contains un.getID.toInt) {
      RACIONAL_MAX_CONNECTIONS
    } else {
      HyParViewJoinTest.activeViewSize
    }
  }

}

object Oracle extends Oracle