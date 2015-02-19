package example.basicGossip
import peersim.core._
import peersim.edsim.EDProtocol
import peersim.cdsim.CDProtocol

class FRProtocol(name: String) extends CDProtocol with EDProtocol{

  def nextCycle(node : Node, pid: Int) = {
      //println("Node: " + node.getIndex + " is FREE RIDER")
  }
  
  def processEvent(node :Node, pid : Int, event: Object) {
  
  }
  

}