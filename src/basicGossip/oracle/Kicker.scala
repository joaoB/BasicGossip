package basicGossip.oracle

trait Kicker extends AllNodes {

  var kicked = Map[Int, Int]()
  var badKicked = Map[Int, Int]()

  def kick(id: Int) =
    if (altruistics contains (id))
      kickAltruistic(id)
    else kickFreeRider(id)


  private def kickFreeRider(id: Int) = {
    kicked = kicked match {
      case map if map.contains(id) =>
        kicked.updated(id, kicked(id) + 1)
      case _ => kicked.updated(id, 1)
    }
  }

  private def kickAltruistic(id: Int) =
    badKicked = badKicked match {
      case map if map.contains(id) =>
        badKicked.updated(id, badKicked(id) + 1)
      case _ => badKicked.updated(id, 1)
    }

}