package basicGossip.messages

import basicGossip.node.Usernode

case class WaitCycles(remainingCycles: Int, sender: Usernode)