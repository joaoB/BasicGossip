package basicGossip.observers

import basicGossip.oracle.Oracle
import basicGossip.node.Usernode
import peersim.core.Network

class MaxHopsObserver(name: String) extends BasicGossipObserver(name) {
  override def run: Boolean = {
    dumpMaxHops
    avgHops
    false
  }

  def dumpMaxHops = {
    Oracle.maxHopInfo match {
      case Some(elem) => println("Max hops -> " + elem.hop)
      case None => //should never happen
    }
  }

  def avgHops = {
    Oracle.avgHops.size match {
      case 0 =>
      case size => println("Avg hops " + Oracle.avgHops.sum / size)
    }

  }
}