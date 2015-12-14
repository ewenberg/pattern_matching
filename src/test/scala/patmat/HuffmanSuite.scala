package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
      assert(weight(t2) === 9)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(hello, world)") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }


  // additional tests
  test("times") {
    new TestTrees {
      val l1 = List('a', 'b', 'a')
      //println(times(l1))
      assert(times(l1).size === 2, "expected only 2 elements")
      assert(times(l1).contains(('a', 2)), "must contain ('a', 2)")
      assert(times(l1).contains(('b', 1)), "must contain ('b', 1)")
    }
  }

  test("makeOrderedLeafList") {
    new TestTrees {
      val freqs = List(('a'-> 9),('b'->5),('c'->2))
      val orderedList = makeOrderedLeafList(freqs)
      assert(orderedList.size === 3, "expected 3 elements")
      assert(orderedList.head == new Leaf('c', 2))
      assert(orderedList.tail.tail.head == new Leaf('a', 9))
    }
  }

  test("decodeChar") {
    new TestTrees {
      assert(decodeChar(t2, List(0,0)) == ('a',List()))
      assert(decodeChar(t2, List(0,1)) == ('b',List()))
      assert(decodeChar(t2, List(1))   == ('d',List()))
    }
  }

  test("decode") {
    new TestTrees {
      assert(decode(t2, List(0,0,0,1,1)) == List('a','b','d'))
      println(Huffman.decodedSecret)
    }
  }

  test("encode") {
    new TestTrees {
      println(encode(t2)(List('a','b','d')))
      assert(encode(t2)(List('a','b','d')) == List(0,0,0,1,1))
    }
  }
}
