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
import scala.collection.mutable.MutableList
import basicGossip.messages.WaitCycles

class ChallengeSolver(name: String) extends CDProtocol with EDProtocol {

  def nextCycle(node: Node, pid: Int) {

    val un = Oracle.getNode(node.getID.toInt)
    un.solveChallenge

    solveAll(un)

    //solveOne(un)  

  }

  private def solveOne(un: Usernode) {
    un.solvingChallenges.filter { x => x._2 <= 0 } map {
      elem =>
        Oracle.getNode(elem._1.toInt).receivedSolvedChallenge(un)
    }
    un.cleanSolvedChallenges
  }

  private def solveAll(un: Usernode) {
    if (!un.solvingChallenges.exists { x => x._2 > 0 } &&
      un.solvingChallenges.size > 0) {
      un.solvingChallenges map {
        elem =>
          Oracle.getNode(elem._1.toInt).receivedSolvedChallenge(un)
      }
      un.cleanSolvedChallenges

    }
  }
  def processEvent(node: Node, pid: Int, event: Object) {
    event match {
      case challenge: ConfirmSolveChallenge => Oracle.getNode(node.getID.toInt).receivedSolvedChallenge(challenge.sender)
      case _ =>
    }

  }

  def sendSimpleMessage(sender: Usernode, receiver: Usernode, message: Any, pid: Int) {
    sender.getProtocol(FastConfig.getTransport(pid)) match {
      case trans: Transport => trans.send(sender, receiver, message, pid)
      case _ => //dont know how to send this message
    }
  }

}