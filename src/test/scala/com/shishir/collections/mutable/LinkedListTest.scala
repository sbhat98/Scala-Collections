package com.shishir.collections.mutable

import org.scalatest._

//noinspection ComparingUnrelatedTypes
class LinkedListTest extends FunSuite {
  val elems = scala.List(3, 4, 6, 1, 2)

  test("List()") {
    val list = new LinkedList()
    assert(list.isEmpty)
    assertThrows[NoSuchElementException] {
      list.head
    }
    assert(list == scala.Nil)
  }

  test("List(elems)") {
    val list = new LinkedList(elems: _*)
    assert(list == elems)
  }

  test("List iterator") {
    val list = new LinkedList(elems: _*)
    for ((x, y) <- list zip elems)
      assert(x == y)
  }

  test("List apply()") {
    val list = new LinkedList(elems: _*)
    for (i <- elems.indices)
      assert(list(i) == elems(i))
  }

  test("List length") {
    val list = new LinkedList(elems: _*)
    assert(list.length == elems.length)
    assert(new LinkedList().length == 0)
  }

  test("List reverse") {
    val list = new LinkedList(elems: _*)
    assert(list.reverse == elems.reverse)
  }

  test("List isEmpty") {
    val list = new LinkedList(elems: _*)
    assert(list.nonEmpty)
    assert(new LinkedList().isEmpty)
  }
}
