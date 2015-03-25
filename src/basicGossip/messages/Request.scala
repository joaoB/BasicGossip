package basicGossip.messages

import basicGossip.node.Usernode

case class Request(ids: List[Int], sender: Usernode)