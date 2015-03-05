package basicGossip.messages

import peersim.core.Node
import basicGossip.node.Usernode

case class Info(value: Int, sender: Usernode, hop: Int)