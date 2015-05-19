package basicGossip.observers

import peersim.core.Control
import basicGossip.oracle.Oracle
import peersim.core.Network

class Test(name: String) extends Control {

  override def execute: Boolean = {
    bidirectionalScore
    bidirectionalHpv
    solvingChallenges
    false
  }

  
  def solvingChallenges = {
    Oracle.allNodesExceptStreamer map {
      node => 
        val puzzles = node.solvingChallenges
        if(puzzles.size > 1) {
        println("BUGGGGGGGGGGGGGG @ Test.solvingChallenges")
        println("Node : " + node.getID + " -> solving " + puzzles.size + " challenges")
        println(puzzles.map(_.sender.getID).toList)
        println("is free rider? "  + Oracle.freeRiders.contains(node.getID))
        }
    }
  }
  
  def bidirectionalHpv = {
    Oracle.nodesHpvProtocolExceptStreamer map {
      node =>
        node._2.neighbors.filter(_ != 0) map {
          id =>
            if (!Oracle.nodeHpvProtocol(id.getID.toInt)._2.neighbors.contains(Network.get(node._1.getID.toInt))) {
              println("BUGGGGGGGGGGGGGG @ Test.bidirectionalHpv")
              val a = Oracle.nodeHpvProtocol(id.getID.toInt)._2.neighbors.map(_.getID)
              val b = node._2.neighbors.map(_.getID)
              println("Node : " + id.getID + " -> " + a.toList)
              println("Node : " + node._1.getID + " -> " + b.toList)
            }
        }
        1
    }
  }

  def bidirectionalScore = {
    Oracle.allNodesExceptStreamer map {
      node =>
        node.scoreList.keySet.filter(_ != 0) map {
          id =>
            if (!Oracle.getNode(id.toInt).scoreList.keySet.contains(node.getID)) {
              println("BUGGGGGGGGGGGGGG @ Test.bidirectionalScore")
              println("Node : " + id.toInt + " -> " + Oracle.getNode(id.toInt).scoreList.keySet)
              println("Node : " + node.getID + " -> " + node.scoreList.keySet)
            }
        }

    }
  }

}