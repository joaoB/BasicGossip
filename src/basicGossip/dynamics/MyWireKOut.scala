package basicGossip.dynamics

import peersim.dynamics.WireGraph
import peersim.graph.GraphFactory
import peersim.graph.Graph
import peersim.core.CommonState
import java.util.Random
import peersim.config.Configuration

class MyWireKOut(name: String) extends WireGraph(name) {

  val k = Configuration.getInt(name + ".k");
  
  def wire(g: Graph) {
    //GraphFactory.wireKOut(g, k, new Random);
  }

}