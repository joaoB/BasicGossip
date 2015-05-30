package basicGossip.protocols

import peersim.cdsim.CDProtocol
import peersim.core.Node
import basicGossip.oracle.Oracle
import peersim.transport.Transport
import basicGossip.node.Usernode
import basicGossip.messages.ConfirmSolveChallenge
import peersim.config.FastConfig
import peersim.edsim.EDProtocol
import basicGossip.node.NodeStatus

class ChallengeSolver(name: String) extends CDProtocol with EDProtocol {

  def nextCycle(node: Node, pid: Int) {
    val un = Oracle.getNode(node.getID.toInt)
    un.solveChallenge

    solveAll(un)
    //solveOne(un)

  }

  //  private def solveOne(un: Usernode) {
  //    un.solvingChallenges.filter { x => x.remainingCycles <= 0 } map {
  //      elem =>
  //        elem.sender.receivedSolvedChallenge(un)
  //    }
  //    un.cleanSolvedChallenges
  //  }

  private def solveAll(un: Usernode) {
    val solving = un.scoreList.filter(_._2.status == NodeStatus.SOLVING)
    if (!solving.exists { x => x._2.score > 0 } &&
      solving.size > 0) {
      solving map {
        elem =>
          val node = Oracle.getNode(elem._1.toInt)
          node.receivedSolvedChallenge(un)
      }
      //un.cleanSolvedChallenges
    }
  }

  def processEvent(node: Node, pid: Int, event: Object) {
    //    event match {
    //      case challenge: ConfirmSolveChallenge => 
    //        println("----------------------------")
    //        Oracle.getNode(node.getID.toInt).receivedSolvedChallenge(challenge.sender)
    //      case _ =>
    //    }

  }

  def sendSimpleMessage(sender: Usernode, receiver: Usernode, message: Any, pid: Int) {
    sender.getProtocol(FastConfig.getTransport(pid)) match {
      case trans: Transport => trans.send(sender, receiver, message, pid)
      case _ => //dont know how to send this message
    }
  }

}