package basicGossip.oracle

import scala.collection.mutable.MutableList
import scala.util.Random

import basicGossip.messages.Info
import basicGossip.node.Usernode
import basicGossip.protocols.AltruisticProtocol
import basicGossip.protocols.FRProtocol
import basicGossip.protocols.GeneralProtocol
import basicGossip.protocols.Link
import hyparview.MyHyParView
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

  val total = 1 until 1000 toList
  val frAmount = (1000 * frPercentage).toInt
  var freeRiders = Random.shuffle(total).take(frAmount)

  val altruistics = total diff freeRiders

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

  def injectFreeRiders = {
    val amount = 300
    freeRiders = Random.shuffle(1 until 1000 toList).take(amount)

    allNodesExceptStreamer filter (node => freeRiders.contains(node.getID)) map {
      x => x.setProtocol(0, new FRProtocol("free rider injected"))
    }
  }

  def addNode = {
    if (freeRiders contains (currentPackage + 1)) {
      addAltruisticNode
    } else {
      addAltruisticNode
    }
  }

  def addAltruisticNode = {
    addNodeAux(new AltruisticProtocol("altruistic"))
  }

  def addFreeRider {
    addNodeAux(new FRProtocol("free rider"))
  }

  private def addNodeAux(protocol: GeneralProtocol) {
    val n = new Usernode("a")
    Network.add(n)

    val node = Oracle.getNode(n.getID.toInt)
    node.setProtocol(0, protocol)
    val nodeID = node.getID.toInt
    val nodeNode = Network.get(nodeID)

    for (id <- 0 until Oracle.MIN_WIN_TO_SEARCH) {
      val connect = Oracle.getNode(Random.nextInt(Network.size))
      val lst = (1 until Network.size toList).diff(List(nodeID)).diff(node.scoreList.keySet toList)

      lst match {
        case Nil => None
        case x => MyHyParView.join(Oracle.getNode(Random.shuffle(x).head), node)
      }
    }

   // println(n.scoreList)
    
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
      Oracle.MIN_WIN_TO_SEARCH
    }
  }

}

object Oracle extends Oracle