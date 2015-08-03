package basicGossip.protocols.Lifting

import basicGossip.messages.Info
import basicGossip.node.Usernode
import basicGossip.protocols.GeneralProtocol.Heavyweight
import basicGossip.oracle.Oracle
import basicGossip.protocols.dissemination.RationalDisseminator
import basicGossip.protocols.dissemination.Disseminator
import basicGossip.protocols.GeneralProtocol.ProtocolName
import basicGossip.protocols.dissemination.LiftingRationalDisseminator

class LiftingRational(name: String) extends Heavyweight(name) {

  override val protocolName = ProtocolName.FR
  override val disseminator: Disseminator = LiftingRationalDisseminator

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

}