package hyparview

import scala.annotation.migration
import scala.util.Random
import basicGossip.node.Usernode
import basicGossip.oracle.Oracle
import basicGossip.node.NodeStatus

class MyHyParView {

  def join(un: Usernode) {
    val nodes = un.scoreList.map(_._1)
    Random.shuffle(nodes).headOption match {
      case Some(elem) => join(un, Oracle.getNode(elem.toInt))
      case None =>
    }
  }

  def join(node: Usernode, connector: Usernode) {

    def joinAux(node: Usernode, connector: Usernode, lenght: Int) {
      lenght match {
        case n if n == 0 => disconnectOneAcceptOther(node, connector)
        case _ =>
          if (canConnect(node, connector)) {
            connector.newNodeSolving(node.getID.toInt) //injects waiting
            node.addNewChallenge(connector) //injects solving scoreList and solvingList
          } else {
            val random = new Random(System.currentTimeMillis)
            random.shuffle(connector.scoreList.filter(_._1 != node.getID).keys).headOption match {
              case Some(nextID) =>
                val nextNode = Oracle.getNode(nextID.toInt)
                joinAux(node, nextNode, lenght - 1)
              case None =>
            }

          }
      }
    }

    joinAux(node, connector, 10)

  }

  private def canConnect(node: Usernode, newMember: Usernode): Boolean = {
    val a = !node.scoreList.keySet.contains(newMember.getID) &&
      node.canAcceptNewNeighbor &&
      node.getID != newMember.getID &&
      !newMember.solvingChallenges.exists(_._1 == node.getID)

    a

  }

  private def disconnectOneAcceptOther(node: Usernode, connector: Usernode) = {
    if (node.getID != connector.getID &&
      !connector.solvingChallenges.exists(_._1 == node.getID)
      && !node.scoreList.keySet.contains(connector.getID)) {
      connector.newNodeSolving(node.getID.toInt) //injects waiting
      node.addNewChallenge(connector) //injects solving scoreList and solvingList
      true
    } else {
      false
    }

  }

}

object MyHyParView extends MyHyParView