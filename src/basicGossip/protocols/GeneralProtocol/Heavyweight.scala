package basicGossip.protocols.GeneralProtocol

import scala.collection.mutable.MutableList

import basicGossip.node.Usernode
import basicGossip.oracle.Oracle
import basicGossip.protocols.dissemination.Disseminator
import basicGossip.protocols.dissemination.LiftingDisseminator
import peersim.core.Network
import utils.RingBuffer

abstract class Heavyweight(name: String) extends GeneralProtocol {

  val historySent: RingBuffer[Long] = new RingBuffer[Long](Oracle.fanout)
  val historyReceived: RingBuffer[Long] = new RingBuffer[Long](50)
  val managers: MutableList[Long] = new MutableList[Long]
  var track: Map[Long, Long] = Map[Long, Long]()
  val blackListed: scala.collection.mutable.Set[Long] = scala.collection.mutable.Set[Long]()

  override def blackList = blackListed

  val baseWin: Int = 0
  def canAcceptNewNeighbor(un: basicGossip.node.Usernode): Boolean = false

  val disseminator: Disseminator = LiftingDisseminator
  val maxWin: Int = 0
  def shouldLookForNewNeighbor(un: basicGossip.node.Usernode): Boolean = false

  val protocolName = ProtocolName.ALT

  override def swapToLifting(un: Usernode) = {
    //we already on lifting
  }

  override def kickFreeRiders(un: Usernode): Unit = {
    val frs = un.freeRiders
    Oracle.allNodesExceptStreamer map {
      node =>
        frs map {
          fr => getLifting(node).blackListed.+=(fr)
          Oracle.kick(fr.toInt)
        }
    }
  }

  def getLifting(un: Usernode) =
    un.behaviorProtocol match {
      case protocol: Heavyweight => protocol
    }

  def traceLog(un: Usernode, ids: Set[Long]) = {
    clearSentHistory(un)
    ids map (updateHistorySent(un, _))
  }

  def saveProposal(un: Usernode, senderID: Long) = {
    updateHistoryReceived(un, senderID)
  }

  def clearSentHistory(un: Usernode) = getLifting(un).historySent.clear

  def updateHistorySent(un: Usernode, id: Long) {
    getLifting(un).historySent.+=(id)
  }

  def updateHistoryReceived(un: Usernode, id: Long) {
    getLifting(un).historyReceived.+=(id)
  }

  def setManagers(un: Usernode) {
    val id = un.getID
    val managersAmount = un.getID + 5
    for (i <- un.getID + 1 until managersAmount) {
      val managerId = i % Network.size
      if (managerId != 0)
        getLifting(un).managers.+=(managerId)
    }
  }

  def validate(that: Usernode) {
    val history = getLifting(that).historySent

    //this counts as a now aswer from the node
    //    if (history.isEmpty) {
    //      alertManagers(that, Oracle.fanout)
    //    }

    if (history.size < Oracle.fanout) {
      alertManagers(that, Oracle.fanout - history.size)
    }

    history map {
      id =>
        val node = Oracle.getNode(id.toInt)
        if (!getLifting(node).historyReceived.contains(that.getID)) {
          alertManagers(that, 1)
        }
    }
  }

  override def freeRiders(un: Usernode) = {
    getLifting(un).track.collect {
      case elem if elem._2 < -100 => elem._1
    }.toList
  }

  def alertManagers(un: Usernode, penalty: Long) = {
    getLifting(un).managers map {
      id =>

        val l = getLifting(Oracle.getNode(id.toInt))
        val score = l.track.getOrElse(un.getID, 0L)
        l.track = l.track.updated(un.getID, score - penalty)
    }

  }

}