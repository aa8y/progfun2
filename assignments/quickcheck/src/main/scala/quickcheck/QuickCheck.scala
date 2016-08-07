package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math.min

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: A =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a1: A, a2: A) =>
    val h = insert(a2, insert(a1, empty))
    findMin(h) == min(a1, a2)
  }

  property("delete1") = forAll { a: A =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  def isOrdered(h: H): Boolean = {
    if (isEmpty(h)) return true
    val currentMin = findMin(h)
    val nextHeap = deleteMin(h)
    if (isEmpty(nextHeap)) return true
    val nextMin = findMin(nextHeap)
    currentMin <= nextMin && isOrdered(nextHeap)
  }

  property("ordered1") = forAll { h: H => isOrdered(h) }

  property("meld1") = forAll { (h1: H, h2: H) => isOrdered(meld(h1, h2)) }

  property("meld2") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val actualMin = min(m1, m2)

    val meldedHeap = meld(h1, h2)
    val meldedMin = findMin(meldedHeap)

    actualMin == meldedMin
  }
}
