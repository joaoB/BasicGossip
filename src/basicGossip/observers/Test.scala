package basicGossip.observers

import peersim.core.Control
import basicGossip.oracle.Oracle
import peersim.core.Network
import basicGossip.node.NodeStatus

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
    //cycle
    //frLess
    //actives
    //s
    //currentPackage

    hops
    //frs

    //waitAttack
    false
  }

  def waitAttack = {
    try {
      val a = Oracle.getNode(2).scoreList
      val actives = a.filter(p => p._2.status == NodeStatus.ACTIVE).size
      val activesNoFrs = a.filter(p => p._2.status == NodeStatus.ACTIVE && !Oracle.freeRiders.contains(p._1)).size
      println(Oracle.currentPackage + " --> " + actives + " ...  " + activesNoFrs)
    } catch {
      case e =>
    }
  }

  def frs = {
    println(Oracle.currentPackage + " --> " + Oracle.badKicked.size)
  }

  def hops = {
    println(Oracle.currentPackage); /*
    /*
    * 
    */
        val a = Oracle.altruistics.map({
          id => (id, Oracle.getNode(id).avgHops)
        }).filter { elem => elem._2 > 5 }
    
        a map {
          elem =>
            if (!Oracle.swap.contains(elem._1))
              Oracle.swap += elem._1
        }
    
        println(Oracle.currentPackage + " --> " + Oracle.swap.size.toFloat / Oracle.altruistics.size)*/

//    val a = Oracle.altruistics.map {
//      id => Oracle.getNode(id).avgHops
//    }
//    val b = a.sum.toFloat / a.size
//    println(Oracle.currentPackage + "  --  " + b)

  }

  def currentPackage = println(Oracle.currentPackage)

  def s = {

    try {
      //for (id <- 1 until 3) {
      val id = 1
      println(Oracle.getNode(id).scoreList.filter(_._2.status == NodeStatus.ACTIVE).map { x => (x._1, x._2.score) }.head)
      //}

      //println(Oracle.currentPackage + ": " + Oracle.kicked.size + " -> " + Oracle.altruisticsAmountOfSentMessages)
    } catch {
      case e =>
    }
  }

  def actives = Oracle.allNodesExceptStreamer map {
    node =>
      val a = node.scoreList.filter(_._2.status == NodeStatus.ACTIVE).size
      if (a > Oracle.MAX_WIN) {
        println("NODES HAVE MORE CONNECTIONS THAN MAXWIN")
      }
  }

  def frLess = {
    Oracle.freeRiders map {
      id =>
        val size = Oracle.getNode(id).scoreList.filter(_._2.status == NodeStatus.ACTIVE).size
        if (size > Oracle.RACIONAL_MAX_CONNECTIONS) {
          println("BUGGGGGGG @ RACIONAIS COM MAIS LIGACOES")
        }
    }
  }

  def cycle = {
    Oracle.altruistics map {
      id =>
        val a = Oracle.getNode(id).solvingChallenges
        println("Node: " + id + " -> " + a)
        try {
          println(Oracle.getNode(a.head._1.toInt).scoreList)
        } catch {
          case e =>
        }
    }
  }

  def solving {
    Oracle.allNodesExceptStreamer map {
      node =>
        val solvingIds = node.solvingChallenges.map { x => x._1 } map {
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
        val sender = node.solvingChallenges.keys
        sender map {
          id =>
            val other = Oracle.getNode(id.toInt)
            if (!other.scoreList.filter(_._2.status == NodeStatus.WAITING).contains(node.getID)) {
              println("SOLVING WITHOUT WAITING")
              println("node: " + node.getID + " solving " + sender)
              println("node: " + id + " waiting " + node.scoreList.filter(_._2.status == NodeStatus.WAITING))
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
      case e: Throwable =>
    }
  }

  def onlyOneInSolving = {
    Oracle.allNodesExceptStreamer map {
      node =>
        val b = node.solvingChallenges.keys
        if (b.toSet.size != b.size) {
          println("BUGGED NODE ")
          println("b.toSet " + b.toSet)
          println("B -> " + b)
          println("BUGGGGGGGGGGGGGG @ Test.onlyOneInSolving")
          println(node.solvingChallenges.keys)
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
        if (node.scoreList.filter(_._2.status == NodeStatus.ACTIVE).size > node.behaviorProtocol.maxWin) {
          println("BUGGGGGGGGGGGGGG @ Test.scoreSize")
          println("NODE: " + node.getID + " score size " + node.scoreList.size)
        }
    }

  }

  def solvingChallenges = {
    Oracle.allNodesExceptStreamer map {
      node =>
        val puzzles = node.solvingChallenges
        if (puzzles.size > node.behaviorProtocol.maxWin) {
          println("BUGGGGGGGGGGGGGG @ Test.solvingChallenges")
          println("Node : " + node.getID + " -> solving " + puzzles.size + " challenges")
          println(puzzles.keys.toList)
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