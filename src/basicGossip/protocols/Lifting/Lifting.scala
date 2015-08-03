
package basicGossip.protocols.Lifting

import scala.annotation.migration

import basicGossip.messages.Info
import basicGossip.node.Usernode
import basicGossip.oracle.Oracle
import basicGossip.protocols.GeneralProtocol.Heavyweight

class Lifting(name: String) extends Heavyweight(name) {

  override def gossipMessage(node: Usernode, info: Info, pid: Int) {
    if (saveInfo(node, info)) {
      val fanout = computeFanout(node, info.sender, info)
      traceLog(node, fanout)
      fanout map {
        id =>
          Oracle.getNode(id.toInt) match {
            case peern if peern.isUp =>
              if (!peern.messageList.contains(info.value)) {
                sendInfo(node, peern, Info(info.value, node, info.hop + 1), pid)
              }
              //node will save the proposal.. even if it was not delivered
              saveProposal(peern, node.getID)

            case _ =>
          }
      }
    }
  }

  override def kickFreeRiders(un: Usernode): Unit = {
    //empty for now
  }

}