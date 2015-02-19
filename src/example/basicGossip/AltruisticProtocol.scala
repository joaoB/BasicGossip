package example.basicGossip

import peersim.core._
import peersim.cdsim.CDProtocol
import peersim.vector.SingleValueHolder
import peersim.edsim.EDProtocol
import peersim.config.FastConfig


class AltruisticProtocol(name: String) extends CDProtocol with EDProtocol {

  def nextCycle(node: Node, pid: Int) = {
    //println("Node: " + node.getIndex + " is altruistic")

  }

  def processEvent(node: Node, pid: Int, event: Object) {
    println("NODE: " + node.getID)
    event match {
      case info: Info => node match {
        case node: Usernode if !node.containsElem(info.value) => saveAndSend(node, info.value, pid)
        case node: Usernode => 
        case _ => ???
      }
      case _ => ???
    }
  }
  
  def saveAndSend(node: Usernode, value: Int, pid: Int){
    node.saveMessage(value)

    val linkable = node.getProtocol(FastConfig.getLinkable(pid))
    for (i <- 0 until BasicGossip.fanout){
      println("a")
    }
    /*
          for (int i = 0; i < fanout; i++) {
        Linkable linkable = (Linkable) node.getProtocol(FastConfig
            .getLinkable(pid));
        if (linkable.degree() > 0) {
          Node peern = linkable.getNeighbor(CommonState.r
              .nextInt(linkable.degree()));

          //System.out.println("streamer sending to " + peern.getIndex());
          
          // XXX quick and dirty handling of failures
          // (message would be lost anyway, we save time)
          if (!peern.isUp())
            return;

          System.out.println("streamer will send to " + peern.getID());
          
          ((Transport) node.getProtocol(FastConfig.getTransport(pid)))
              .send(node, peern, new Info(info, node),
                  pid);
        }

      }*/
    
  }

}