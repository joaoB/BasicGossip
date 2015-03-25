//package basicGossip.protocols
//
//import basicGossip.messages.Info
//import basicGossip.messages.join.JoinConfirm
//import basicGossip.messages.join.JoinRequest
//import basicGossip.messages.join.JoinResponse
//import peersim.core.Node
//import basicGossip.oracle.Oracle
//import basicGossip.node.Usernode
//import basicGossip.messages.join.JoinRequest
//import peersim.transport.Transport
//import peersim.config.FastConfig
//import peersim.core.Network
//import basicGossip.messages.join.JoinConfirm
//
//class AltruisticWithAggregation(name: String) extends AltruisticProtocol(name) {
//
//  override def processEvent(node: Node, pid: Int, event: Object) = {
//    event match {
//      case info: Info => processInfo(node, pid, info)
//      case request: JoinRequest => receiveJoinRequest(Oracle.getNode(node.getID.toInt), request, pid)
//      case response: JoinResponse => receiveJoinRespose(Oracle.getNode(node.getID.toInt), response)
//      case confirm: JoinConfirm =>
//      case _ =>
//    }
//  }
//
//  private def receiveJoinConfirm(un: Usernode, confirm: JoinConfirm) = {
//    if(confirm.approval) {
//      un.receivePositiveConfirm(confirm.sender.getID.toInt)
//    } else {
//      //receive negative confirm.. remove id from waiting confirm list
//    }
//    
//  }
//  
//  private def sendJoinConfirm(un: Usernode, nid: Int, pid: Int) = {
//    val confirm = un.addNewNeighbor(nid)
//    val joinConfirm = JoinConfirm(confirm, un)
//    sendSimpleMessage(un, Oracle.getNode(nid), joinConfirm, pid)
//  }
//
//  private def receiveJoinRespose(un: Usernode, response: JoinResponse) = {
//
//  }
//
//  private def sendJoinResponse(un: Usernode, nid: Int, pid: Int) = {
//    un.addWaitingConfirm(nid)
//    val receiver = Oracle.getNode(nid)
//    sendSimpleMessage(un, receiver, JoinResponse(un), pid)
//  }
//
//  private def receiveJoinRequest(un: Usernode, request: JoinRequest, pid: Int) = {
//    if (un.canAcceptJoinRequest) {
//      sendJoinResponse(un, request.id, pid)
//    } else {
//      forwardJoinRequest(un, request, pid)
//    }
//  }
//
//  private def forwardJoinRequest(un: Usernode, joinRequest: JoinRequest, pid: Int) = {
//    joinRequest.hop match {
//      case n if n < 10 /* 10 = max hop*/ => un.altruisticsNeighbors map {
//        elem =>
//          sendSimpleMessage(un, Oracle.getNode(elem.toInt), joinRequest.copy(hop = joinRequest.hop + 1), pid)
//      }
//      case _ => //do not forward
//    }
//  }
//
//}