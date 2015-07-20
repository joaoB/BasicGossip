package basicGossip.protocols.Lifting

import scala.annotation.migration

import basicGossip.messages.Info
import basicGossip.node.Usernode
import basicGossip.oracle.Oracle
import basicGossip.protocols.GeneralProtocol.Heavyweight

class Lifting(name: String) extends Heavyweight(name) {

  override def gossipMessage(node: Usernode, info: Info, pid: Int) {
    if (saveInfo(node, info)) {
      computeFanout(node, info.sender) map {
        id =>
          Oracle.getNode(id.toInt) match {
            case peern if peern.isUp =>
              if (!peern.messageList.contains(info.value)) {
                updateHistorySent(node, id)
                updateHistoryReceived(peern, node.getID)
                sendInfo(node, peern, Info(info.value, node, info.hop + 1), pid)
              }
            case _ =>
          }
      }
    }
  }

  override def kickFreeRiders(un: Usernode): Unit = {
    //empty for now
  }

  override def swapToLifting(un: Usernode) = {
    //we already on lifting
  }

}