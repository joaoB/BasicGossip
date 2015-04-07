package basicGossip.observers

import basicGossip.node.Usernode
import basicGossip.protocols.BasicGossip
import peersim.core.Network

class TestingObserver(name: String) extends BasicGossipObserver(name) {
 
  override def run: Boolean = {
    false
  }
 
   private def test = {
    Network.get(0) match {
      case streamer: Usernode => println(streamer)
    }

    for (i <- 1 until Network.size) {
      print("Node: " + i + " ->")
      Network.get(i) match {
        case un: Usernode => println(un.scoreList.filter(_._2 > 0))
      }
    }
  }

  private def test3 = {
    //does any node has a connection to itself????
    //guess no...
    for (i <- 0 until Network.size) {
      print("Node: " + i + " ->")
      Network.get(i) match {
        case un: Usernode => un.scoreList.find(_._1 == un.getID) match {
          case Some(elem) => println("BINGO!!!! PIMBA NE BELHA")
          case None => println
        }
      }
    }
  }

  private def checkSimmetryScores = {
    for (i <- 0 until Network.size) {
      print("Node: " + i + " ->")
      Network.get(i) match {
        case un: Usernode => println(un.scoreList)

      }
    }
  }
  


  private def test2 = {
    //the node with less % is also the node with less connections?
    // YES!!!
    for (i <- 1 until Network.size) {
      print("Node: " + i + " ->")
      Network.get(i) match {
        case un: Usernode => println(un.scoreList.size + " " + un.messageList.size.toFloat / BasicGossip.cycles)
      }
    }
  }

}