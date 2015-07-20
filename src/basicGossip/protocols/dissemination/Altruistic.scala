package basicGossip.protocols.dissemination

import basicGossip.node.Usernode
import peersim.core.Node
import basicGossip.node.NodeStatus
import basicGossip.oracle.Oracle
import basicGossip.messages.Info

class Altruistic extends Disseminator {
  override def computeFanout(gossiper: Usernode, sender: Node): Set[Long] = {
    (gossiper.scoreList.filter(x => x._1 != 0 && x._1 != sender.getID && x._2.status == NodeStatus.ACTIVE) filter {
      case (id, neighbor) if neighbor.score >= Oracle.baseRank => aboveBaseRank
      case (id, neighbor) if Oracle.baseRank > neighbor.score && neighbor.score >= Oracle.FR_THRESHOLD => belowBaseRank(neighbor.score)
      case (id, neighbor) if neighbor.score < Oracle.FR_THRESHOLD => false
    }).keySet
  }

  
    override def computeFanout(gossiper: Usernode, sender: Node, info: Info): Set[Long] = {
    (gossiper.scoreList.filter(x => x._1 != 0 && x._1 != sender.getID && x._2.status == NodeStatus.ACTIVE) filter {
     // case (id, neighbor) if neighbor.score > 13 && !Oracle.getNode(id.toInt).messageList.contains(info.value) => true
      case (id, neighbor) if neighbor.score >= Oracle.baseRank => aboveBaseRank
      case (id, neighbor) if Oracle.baseRank > neighbor.score && neighbor.score >= Oracle.FR_THRESHOLD => belowBaseRank(neighbor.score)
      case (id, neighbor) if neighbor.score < Oracle.FR_THRESHOLD => false
    }).keySet
  }
  
}

object AltruisticDisseminator extends Altruistic