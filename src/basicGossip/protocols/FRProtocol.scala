package basicGossip.protocols

import basicGossip.messages.Info
import basicGossip.node.Usernode
import basicGossip.oracle.Oracle

class FRProtocol(name: String) extends AltruisticProtocol(name) with GeneralProtocol {

  override def gossipMessage(node: Usernode, info: Info, pid: Int) {
    saveInfo(node, info)
  }
//  override def shouldLookForNewNeighbor(un: Usernode): Boolean = {
//    Oracle.nodeHpvProtocol(un.getID.toInt)._2.neighbors.size < 12 &&
//      un.waitingConfirm.size + un.scoreList.size + un.solvingChallenges.size < 12
//
//  }

}