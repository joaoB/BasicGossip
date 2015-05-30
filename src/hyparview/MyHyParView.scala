package hyparview

import scala.annotation.migration
import scala.util.Random
import basicGossip.node.Usernode
import basicGossip.oracle.Oracle
import basicGossip.node.NodeStatus

class MyHyParView {

  val MAX_CONNECTIONS = 15

  def join(node: Usernode, newMember: Usernode) {

    def joinAux(node: Usernode, newMember: Usernode, lenght: Int) {
      lenght match {
        case n if n == 0 => disconnectOneAcceptOther(node, newMember)
        case _ =>
          if (canConnect(node, newMember)) {
            node.newNodeSolving(newMember.getID.toInt)
            newMember.addNewChallenge(node)
          } else {
            val nextID = Random.shuffle(node.scoreList.keys).head
            val nextNode = Oracle.getNode(nextID.toInt)
            joinAux(nextNode, newMember, lenght - 1)
          }
      }
    }

    joinAux(node, newMember, 10)

  }

  private def canConnect(node: Usernode, newMember: Usernode): Boolean = {
    val b = node.getID != newMember.getID
    val c = !newMember.scoreList.filter(_._2.status == NodeStatus.SOLVING).exists(_._1 == node.getID)
    val d = node.scoreList.size < MAX_CONNECTIONS

    b && c && d

  }

  private def disconnectOneAcceptOther(node: Usernode, newMember: Usernode) = {
    val a = node.getID != newMember.getID
    val b = !node.scoreList.keySet.contains(newMember.getID)
    if (a && b) {
      node.newNodeSolving(newMember.getID.toInt)
      newMember.addNewChallenge(node)
    }

  }
}
object MyHyParView extends MyHyParView