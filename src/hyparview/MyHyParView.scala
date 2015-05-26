package hyparview

import basicGossip.node._
import scala.util.Random
import basicGossip.oracle.Oracle

class MyHyParView {

  val MAX_CONNECTIONS = 15

  def join(node: Usernode, newMember: Usernode) {

    def joinAux(node: Usernode, newMember: Usernode, lenght: Int) {
      lenght match {
        case n if n == 0 => disconnectOneAcceptOther(node, newMember)
        case _ =>
          if (canConnect(node, newMember)) {
            node.newNodeSolving(newMember.getID)
            newMember.addChallenge(node)
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
    !node.scoreList.keySet.contains(newMember.getID) &&
      (node.scoreList.values.toSeq.filter{
      x => node.isActive(x.status)
    }).size < MAX_CONNECTIONS 

  }

  private def disconnectOneAcceptOther(node: Usernode, newMember: Usernode) = {

  }

}