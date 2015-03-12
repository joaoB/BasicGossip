package utils

import scala.util.Random

class DistinctRandom {
  def sample[A](itms: List[A], sampleSize: Int) = {
    def collect(vect: Vector[A], sampleSize: Int, acc: List[A]): List[A] = {
      if (sampleSize == 0) acc
      else {
        val index = Random.nextInt(vect.size)
        collect(vect.updated(index, vect(0)) tail, sampleSize - 1, vect(index) :: acc)
      }
    }

    collect(itms toVector, sampleSize, Nil)
  } //> sample: [A](itms: List[A], sampleSize: Int)List[A] 

}

object DistinctRandom extends DistinctRandom

