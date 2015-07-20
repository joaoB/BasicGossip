package basicGossip.protocols.GeneralProtocol

import basicGossip.protocols.dissemination.LiftingDisseminator
import scala.collection.mutable.MutableList
import basicGossip.protocols.dissemination.Disseminator
import basicGossip.node.Usernode
import peersim.core.Network

abstract class Heavyweight(name: String) extends GeneralProtocol {

  val historySent: MutableList[Long] = new MutableList[Long]
  val historyReceived: MutableList[Long] = new MutableList[Long]
  val managers: MutableList[Long] = new MutableList[Long]

  val baseWin: Int = 0
  def canAcceptNewNeighbor(un: basicGossip.node.Usernode): Boolean = false

  val disseminator: Disseminator = LiftingDisseminator
  val maxWin: Int = 0
  def shouldLookForNewNeighbor(un: basicGossip.node.Usernode): Boolean = false

  val protocolName = ProtocolName.LIFTING

  def getLifting(un: Usernode) =
    un.behaviorProtocol match {
      case protocol: Heavyweight => protocol
    }

  def updateHistorySent(un: Usernode, id: Long) {
    getLifting(un).historySent.+=(id)
  }

  def updateHistoryReceived(un: Usernode, id: Long) {
    getLifting(un).historyReceived.+=(id)
  }

  def setManagers(un: Usernode) {
    val id = un.getID
    val managersAmount = un.getID + 25
    for (i <- un.getID until managersAmount) {
      val managerId = i % Network.size
      if (managerId != 0)
        getLifting(un).managers.+=(managerId)
    }
  }

}