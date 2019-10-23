package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {


  lazy val genHeap: Gen[H] = oneOf(
      const(empty),
      for {
        value <- arbitrary[Int]
        h <- oneOf(const(empty), genHeap)
      } yield insert(value, h)/*,
      for {
        h1 <- genHeap
        h2 <- genHeap
      } yield meld(h1, h2)*/
    )
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("smallest of two elements") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == Math.min(a, b)
  }

  property("insert 1 element -> delete 1 element -> empty") = forAll { (a: Int) =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  def isSorted(h: H, n: Int): Boolean = {
    if (isEmpty(h)) true
    else if (n <= findMin(h)) isSorted(deleteMin(h), findMin(h))
    else false
  }

  def toList(h: H): List[Int] = 
    if (isEmpty(h)) Nil else findMin(h)::toList(deleteMin(h))

  property("heap is sorted while finding and deleting minima") = forAll { (h: H) =>
    isSorted(h, Int.MinValue)
  }

  property("find minima after melding of 2 heaps") = forAll { (h1: H, h2: H) => 
    val melded = meld(h1, h2)

    if (isEmpty(h1) && isEmpty(h2)) isEmpty(melded)
    else if (!isEmpty(h1) && !isEmpty(h2))
      findMin(h1) == findMin(melded) || findMin(h2) == findMin(melded)
    else if (isEmpty(h1)) findMin(h2) == findMin(melded)
    else findMin(h1) == findMin(melded)
  }

  property("melding preserves order") = forAll { (h1: H, h2: H) => 
    val melded = meld(h1, h2) 
    isSorted(melded, Int.MinValue)
  }

  property("number of elements is preserved when melding") = forAll { (h1: H, h2: H) =>
    val melded = meld(h1, h2)

    def count(h: H): Int = if (isEmpty(h)) 0 else 1 + count(deleteMin(h))

    count(h1) + count(h2) == count(melded)
  }

  property("insert element > minimum must preserve minima") = forAll { (h: H) =>
    if (isEmpty(h)) true
    else if (findMin(h) == Int.MaxValue) true
    else {
      val min = findMin(h)
      val inserted = insert(min + 1, h)
      findMin(inserted) == min
    }
  }

  property("insert element < minimum change minima") = forAll { (h: H) => 
    if (isEmpty(h)) true
    else if (findMin(h) == Int.MinValue) true
    else {
      val min = findMin(h) - 1
      val inserted = insert(min, h)
      findMin(inserted) == min
    }
  }

  property("meld must be commutative") = forAll { (h1: H, h2: H) => 
    val r = meld(h1, h2)
    val l = meld(h2, h1)
    toList(r) == toList(l)
  }

  property("meld must be associative") = forAll { (h1: H, h2: H, h3: H) => 
    val r = meld(h1, meld(h2, h3))
    val l = meld(h3, meld(h1, h2))
    toList(r) == toList(l)
  }
}
