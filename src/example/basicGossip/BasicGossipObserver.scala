package example.basicGossip

import peersim.core._

class BasicGossipObserver(name: String) extends Control {

  def execute = {
    for(i <- 0 until Network.size()){
       Network.get(i) match{
         case node : Usernode => node.dumpMessageList
         case _ => ???
       }
      
    }
    
    false
  }
  
}