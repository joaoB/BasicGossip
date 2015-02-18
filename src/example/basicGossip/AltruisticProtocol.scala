package example.basicGossip

import peersim.core._
import peersim.cdsim.CDProtocol
import peersim.vector.SingleValueHolder
import peersim.edsim.EDProtocol


class AltruisticProtocol(name: String) extends CDProtocol with EDProtocol{

  def nextCycle(node : Node, pid: Int) = {
    System.out.println("PID " + pid);
    //println("altruistic node.. yupie")
  
  }
  
  def processEvent(node :Node, pid : Int, event: Object) {
  
  }
  

}