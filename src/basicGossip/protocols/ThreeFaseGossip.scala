package basicGossip.protocols

import basicGossip.node.Usernode
import basicGossip.messages.Info
import basicGossip.messages.Propose
import basicGossip.messages.Request
import peersim.core.Node
import peersim.transport.Transport
import peersim.config.FastConfig
import basicGossip.oracle.Oracle
import scala.util.Random

class ThreeFaseGossip(name: String) extends AltruisticProtocol(name) {

  override def shouldLookForNewNeighbor(un: Usernode): Boolean = {
    Oracle.nodeHpvProtocol(un.getID.toInt)._2.neighbors.size < Oracle.minWindow && un.solvingChallenges.size == 0
  }

  override def processEvent(node: Node, pid: Int, event: Object) = {
    event match {
      case info: Info => processInfo(node, pid, info)
      case propose: Propose => processPropose(Oracle.getNode(node.getID.toInt), pid, propose)
      case request: Request => processRequest(Oracle.getNode(node.getID.toInt), pid, request)
      case _ => ???
    }
  }

  override def gossipMessage(node: Usernode, info: Info, pid: Int) {
    if (saveInfo(node, info)) {
      val linkable = Oracle.getLinkable(node)
      val proposedIds = generateProposeIds(node);

      computeFanout(node, info.sender) map {
        id =>
          if (linkable.degree() > 0) {
            val peern = linkable.getNeighborById(id) match {
              case Some(peern) if peern.isUp =>
                node.getProtocol(FastConfig.getTransport(pid)) match {
                  case trans: Transport => sendPropose(trans, node, peern,
                    proposedIds, pid)
                  case _ => ???
                }
              case _ =>
            }

          }

      }
    }
  }

  override def canAcceptNewNeighbor(un: Usernode) = Oracle.getLinkable(un).neigh.size < Oracle.fanout

  private def generateProposeIds(node: Usernode): List[Int] = {
    //  node.messageList.toList.filter(_.isDefined).map(_.get.value).takeRight(1)
    node.messageList.toList.takeRight(1)

  }

  def processPropose(node: Usernode, pid: Int, propose: Propose) = {
    val wantedIds = generateRequest(node, propose)
    //send to the guy who proposed what we want
    sendWantedList(node, propose.sender, wantedIds, pid)

  }

  def generateRequest(un: Usernode, propose: Propose): List[Int] = {
    //compute the ids that un wants
    // val messagesIds = un.messageList.toList.filter(_.isDefined).map(_.get.value).takeRight(1)
    val messagesIds = un.messageList.toList.takeRight(1)

    propose.ids.diff(messagesIds)

  }

  def processRequest(node: Usernode, pid: Int, request: Request) {
    //some node has this request
    request.ids map {
      id =>
        node.messageList.contains(id) match {
          case /*Some(info)*/ true => serveRequest(node, request.sender, Info(id, request.sender, 0), pid)
          case _ => println("some guy requested something that he was not proposed") //some guy requested something that he was not proposed
        }
    }

  }

  def sendWantedList(sender: Usernode, receiver: Usernode, ids: List[Int], pid: Int) {
    val transport = sender.getProtocol(FastConfig.getTransport(pid))
    sender.getProtocol(FastConfig.getLinkable(pid)) match {
      case link: Link if receiver.isUp =>
        transport match {
          case trans: Transport if ids.size > 0 => sendRequest(trans, sender, receiver, Request(ids, sender), pid)
          case _ =>
        }
      case _ =>
    }
  }

  def sendPropose(trans: Transport, sender: Usernode, receiver: Node, ids: List[Int], pid: Int) {
    //Oracle.incSentMessages
    trans.send(sender, receiver, Propose(ids, sender), pid)
  }

  private def sendRequest(trans: Transport, node: Usernode, peern: Node, request: Request, pid: Int) {
    //Oracle.incSentMessages
    trans.send(node, peern, request, pid)
  }

  def serveRequest(sender: Usernode, receiver: Usernode, info: Info, pid: Int) {
    val linkable = sender.getProtocol(FastConfig.getLinkable(pid))
    linkable match {
      case link: Link =>
        val peern = link.getNeighborById(receiver.getID) match {
          case Some(peern) if peern.isUp =>
            sendInfo(sender, peern, Info(info.value, sender, info.hop + 1), pid)
          case _ =>
        }

    }
  }

}