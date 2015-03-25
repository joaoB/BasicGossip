package utils

import scala.collection.mutable.MutableList
import scala.util.Random

import basicGossip.oracle.Oracle
import peersim.core.Network

case class User(id: Int) {
  var vizinhos: MutableList[Long] = MutableList()
}

class Organize {

  val size = Network.size
  val window = Oracle.fanout + 1
  val users = for (id <- 0 until size) yield new User(id)

  def add(elem: User, id: Int){
    id match {
      case i if i != elem.id && elem.vizinhos.size < window && users(i).vizinhos.size < window && !elem.vizinhos.contains(id) =>
        elem.vizinhos += i
        users(i).vizinhos += elem.id
      case _ =>
    }
  }
  
  def a = {
    users map {
      //case elem if elem.id == size - 1 => //last node
      case elem if elem.vizinhos.size < window =>
        DistinctRandom.sample(0 until size toList, window) map {
          id => add(elem, id)
        }
        elem.vizinhos match {
          case v if v.size < window => Random.shuffle((users filter (_.vizinhos.size < window))).take(window - v.size) map {
            toadd => add (elem, toadd.id)
          }
          case _ =>
        }
      case _ =>
    }
    users
  }

  
}

object Organize extends Organize


