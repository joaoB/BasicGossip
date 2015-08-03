package basicGossip.protocols.GeneralProtocol

import basicGossip.node.Usernode
import basicGossip.oracle.Oracle
import basicGossip.node.NodeStatus._

abstract class Lightweight(name: String) extends GeneralProtocol {

  override def freeRiders(un: Usernode) =
    un.scoreList.collect {
      case elem if elem._2.score <= Oracle.FR_THRESHOLD && elem._2.status == ACTIVE => elem._1
    } toList

  override def kickFreeRiders(un: Usernode): Unit = 
    un.freeRiders map {
      fr =>
        Oracle.kick(fr.toInt)
        un.removeFromScoreList(fr)
        Oracle.getNode(fr.toInt).removeFromScoreList(un.getID)
    }
  
    
    override def blackList = scala.collection.mutable.Set[Long]()
}