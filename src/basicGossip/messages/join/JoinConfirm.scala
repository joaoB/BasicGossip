package basicGossip.messages.join

import basicGossip.node.Usernode

case class JoinConfirm(approval: Boolean, sender: Usernode)