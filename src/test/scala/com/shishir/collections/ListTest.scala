package com.shishir.collections

import org.scalatest._

//noinspection ComparingUnrelatedTypes
class ListTest extends FunSuite {

  val elems = scala.List(3, 4, 6, 1, 2)

  test("List()") {
    val list = List()
    assert(list.isEmpty)
    assertThrows[NoSuchElementException] {
      list.head
    }
    assert(list == Nil)
  }

  test("List(elems)") {
    val list = List(elems: _*)
    assert(list == elems)
  }

  test("List unapply()") {
    val emptyList = Nil
    val oneElemList = 2 :: Nil
    val genericList = List(elems: _*)

    emptyList match {
      case Nil => succeed
      case _ => fail()
    }
    oneElemList match {
      case List(_) => succeed
      case _ => fail()
    }
    genericList match {
      case List(_, _) => succeed
      case _ => fail()
    }
  }

  test("List ::") {
    val list = 3 :: 4 :: 6 :: 1 :: 2 :: Nil
    assert(list == elems)
  }

  test("List iterator") {
    val list = List(elems: _*)
    for ((x, y) <- list zip elems)
      assert(x == y)
  }

  test("List apply()") {
    val list = List(elems: _*)
    for (i <- elems.indices)
      assert(list(i) == elems(i))
  }

  test("List length") {
    val list = List(elems: _*)
    assert(list.length == elems.length)
    assert(Nil.length == 0)
  }

  test("List reverse") {
    val list = List(elems: _*)
    assert(list.reverse == elems.reverse)
  }

  test("List head") {
    val list = List(elems: _*)
    assert(list.head == 3)
    assertThrows[NoSuchElementException](Nil.head)
  }

  test("List tail") {
    val list = List(elems: _*)
    assert(list.tail == elems.tail)
    assertThrows[UnsupportedOperationException](Nil.tail)
  }

  test("List isEmpty") {
    val list = List(elems: _*)
    assert(!list.isEmpty)
    assert(Nil.isEmpty)
  }
}
