package basicGossip.protocols

import basicGossip.node.Usernode
import basicGossip.messages.Info
import basicGossip.messages.Propose
import basicGossip.messages.Request
import peersim.core.Node
import peersim.transport.Transport
import peersim.config.FastConfig
import basicGossip.oracle.Oracle

class ThreeFaseGossip(name: String) extends GeneralProtocol {

  override def processEvent(node: Node, pid: Int, event: Object) = {
    event match {
      case info: Info => processInfo(node, pid, info)
      case propose: Propose => processPropose(node, pid, propose)
      case request: Request => processRequest(node, pid, request)
      case _ => ???
    }
  }

  override def gossipMessage(node: Usernode, info: Info, pid: Int) {
    if (!saveInfo(node, info)) {
      val linkable = node.getProtocol(FastConfig.getLinkable(pid))
      node.randomGossip(BasicGossip.fanout, info.sender) map {
        id =>
          linkable match {
            case link: Link =>
              if (link.degree() > 0) {
                val peern = link.getNeighborById(id) match {
                  case Some(peern) if peern.isUp =>
                    node.getProtocol(FastConfig.getTransport(pid)) match {
                      case trans: Transport => sendPropose(trans, node, peern,
                        node.messageList.takeRight(5).map(_.value).toList, pid)
                      case _ => ???
                    }
                  case _ =>
                }

              }
          }
      }
    }
  }

  def processPropose(node: Node, pid: Int, propose: Propose) = {
    node match {
      case un: Usernode =>
        val wantedIds = generateRequest(un, propose)
        //send to the guy who proposed what we want
        sendWantedList(un, propose.sender, wantedIds, pid)
      case _ =>
    }
  }

  def generateRequest(un: Usernode, propose: Propose): List[Int] = {
    //compute the ids that un wants
    val messagesIds = un.messageList.map(_.value)
    propose.ids.diff(messagesIds)

  }

  def processRequest(node: Node, pid: Int, request: Request) {
    //some node has this request
    node match {
      case un: Usernode => request.ids map {
        id =>
          un.messageList.find(_.value == id) match {
            case Some(elem) => serveRequest(un, request.sender, elem, pid)
            case None => //some guy requested something that he was not proposed
          }
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
            sender.getProtocol(FastConfig.getTransport(pid)) match {
              case trans: Transport =>
                sendInfo(trans, sender, peern, Info(info.value, sender, info.hop + 1), pid)
              case _ => ???
            }
          case _ =>
        }

    }
  }

}