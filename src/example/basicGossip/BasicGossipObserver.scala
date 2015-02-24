package example.basicGossip

import peersim.core._
import example.basicGossip.oracle.Oracle
import example.basicGossip.protocols.BasicGossip

class BasicGossipObserver(name: String) extends Control {

  def execute = {
    for (i <- 0 until Network.size) {
      Network.get(i) match {
        case node: Usernode => //node.dumpPercentageOfMessage
        //case node: Usernode => node.dumpAmoutOfMessage
        case _ => ???
      }

      val percentages = (for (i <- 1 until Network.size) yield Network.get(i)) map {
        elem =>
          elem match {
            case un: Usernode => un.messageList.size.toFloat / (BasicGossip.cycles - 1)
          }
      }
      println("Min percentage: " + percentages.min)
      println("Max percentage: " + percentages.max)

      //      print("Max Hop Info -> ")
      //      println(Oracle.maxHopInfo.hop)     
    }
    /*println("NODE 42 scorelist")
    Network.get(42) match { case a: Usernode => a.scoreList map(x => println (x._1 + " -> " + x._2)) }
*/
    //TODO: meter "<90" a ir buscar o 90 ao file.. o score de Free Riders tambem
    /*for (i <- 0 until Network.size()) {
      Network.get(i) match {
        case node: Usernode => 
          println("NODE: " + i + " detected -> " + node.scoreList.filter(x => x._2 < -2 && x._1 < 90 && x._1 > 0).size + " positive free riders")
          println("NODE: " + i + " detected -> " + node.scoreList.filter(x => x._2 < -2 && x._1 >= 90).size + " false free riders")
        case _ => ???
      }
    }*/

    false
  }

}