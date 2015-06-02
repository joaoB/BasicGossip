package basicGossip.observers

import peersim.core.Control
import basicGossip.oracle.Oracle
import peersim.core.Network
import basicGossip.node.NodeStatus
import org.omg.PortableInterceptor.ACTIVE

class Test(name: String) extends Control {

  override def execute: Boolean = {
    //bidirectionalScore
    //scoreSize
    //onlyOneInSolving
    //bidirectionalHpv
    //solvingChallenges
    //oneActive
    //puzzlesAmount
    //solveNwaiting
    //solving
    false
  }

  def solving {
    Oracle.allNodesExceptStreamer map {
      node =>
        val solvingIds = node.solvingChallenges.map { x => x.sender.getID } map {
          key =>
            if (!node.scoreList.filter(_._2.status == NodeStatus.SOLVING).map(_._1).toList.contains(key)) {
              println("BUGGGG SOLVING LIST DOES NOT MATCH SCORELIST SOLVING")
            }
        }
    }
  }

  def solveNwaiting = {
    val allPuzzles = Oracle.allNodesExceptStreamer map {
      node =>
        val sender = node.solvingChallenges.map(_.sender)
        sender map {
          x =>
            if (!x.scoreList.filter(_._2.status == NodeStatus.WAITING).contains(node.getID)) {
              println("SOLVING WITHOUT WAITING")
              println("node: " + node.getID + " solving " + sender.map(_.getID))
              println("node: " + x.getID + " waiting " + x.scoreList.filter(_._2.status == NodeStatus.WAITING))
            }
        }
    }
  }

  def puzzlesAmount = {
    //    Oracle.allNodesExceptStreamer map {
    //      node => 
    //        val a = node.solvingChallenges.map(_.sender.getID) toList
    //        val b = node.solvingChallenges.map(_.sender.getID) toSet
    //        
    //        if (a.size != b.size){
    //          println("ashduahdusahdu")
    //        }
    //    }

    try {
      val size = for (id <- 1000 until 1100) yield Oracle.getNode(id).solvingChallenges.size

      println("NEW SOLVING " + size.sum)
      
    } catch {
      case e: Throwable =>
    }

    val allPuzzles = Oracle.allNodesExceptStreamer map {
      node =>
        val a = node.solvingChallenges.size
        a
    }
    println(Oracle.currentPackage + " -> " + allPuzzles.sum.toFloat / Network.size)

    try {
      println("max puzzles size " + allPuzzles.max)

      //      println("MAX " + a.max)
      //
      //      println("MIN " + a.min)
    } catch {
      case e: Throwable=>
    }
  }

  def onlyOneInSolving = {
    Oracle.allNodesExceptStreamer map {
      node =>
        val b = node.solvingChallenges map (_.sender.getID)
        if (b.toSet.size != b.size) {
          println("BUGGED NODE ")
          println("b.toSet " + b.toSet)
          println("B -> " + b)
          println("BUGGGGGGGGGGGGGG @ Test.onlyOneInSolving")
          println(node.solvingChallenges.map(_.sender.getID))
        }
    }

  }

  def oneActive = {
    Oracle.allNodesExceptStreamer map {
      node =>
        val a = node.scoreList.filter(_._2.status == NodeStatus.ACTIVE)
        if (a.size == 1) {
          println("NODE : " + node.getID)
          println("scorelist size " + node.scoreList.size)
          println(node.scoreList)
        }
    }

  }

  def scoreSize = {
    Oracle.allNodesExceptStreamer map {
      node =>
        if (node.scoreList.filter(_._2.status == NodeStatus.ACTIVE).size > 15) {
          println("BUGGGGGGGGGGGGGG @ Test.scoreSize")
          println("NODE: " + node.getID + " score size " + node.scoreList.size)
        }
    }

  }

  def solvingChallenges = {
    Oracle.allNodesExceptStreamer map {
      node =>
        val puzzles = node.solvingChallenges
        if (puzzles.size > 1) {
          println("BUGGGGGGGGGGGGGG @ Test.solvingChallenges")
          println("Node : " + node.getID + " -> solving " + puzzles.size + " challenges")
          println(puzzles.map(_.sender.getID).toList)
          println("is free rider? " + Oracle.freeRiders.contains(node.getID))
        }
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

        node.scoreList.filter(x => x._1 != 0 && x._2.status == NodeStatus.ACTIVE) map {
          id =>
            if (!Oracle.getNode(id._1.toInt).scoreList.filter(_._2.status == NodeStatus.ACTIVE).contains(node.getID)) {
              println("BUGGGGGGGGGGGGGG @ Test.bidirectionalScore NO ACTIVE MATCH")
              println("Node : " + id._1.toInt + " -> " + Oracle.getNode(id._1.toInt).scoreList.keySet)
              println("Node : " + node.getID + " -> " + node.scoreList.keySet)
            }
        }

    }
  }

}