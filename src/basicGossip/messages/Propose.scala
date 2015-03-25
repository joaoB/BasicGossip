package basicGossip.messages

import basicGossip.node.Usernode

case class Propose(ids: List[Int], sender: Usernode)