package com.shishir.collections

import org.scalatest._

class BSTTest extends FunSuite {

  val elems = List(1, 3, 5, 8, 0)

  test("BST()") {
    val tree = BST()
    assert(tree.isEmpty)
  }

  test("BST(elems)") {
    val tree = BST(elems: _*)
    for (x <- 0 to 10)
      if (elems.contains(x)) assert(tree.contains(x))
      else assert(!tree.contains(x))
  }

  test("BST add") {
    var tree = BST(elems: _*)
    assert(!tree.contains(4))
    tree += 4
    assert(tree.contains(4))
  }

  test("BST remove") {
    var tree = BST(elems: _*)
    assert(tree.contains(1))
    tree -= 1
    assert(!tree.contains(1))
    assert(tree.contains(0))
    tree -= 0
    assert(!tree.contains(0))
  }

  test("BST iterator") {
    val tree = BST(elems: _*)
    assert(tree.iterator sameElements elems.sorted.iterator)
  }

  test("BST isEmpty") {
    val empty = BST()
    assert(empty.isEmpty)
    val nonempty = BST(1, 2, 4)
    assert(!nonempty.isEmpty)
  }

  test("BST size") {
    val tree = BST(elems: _*)
    assert(tree.size == elems.length)
    assert(BST().size == 0)
  }

  test("BST traversal index") {
    val tree = BST(elems: _*)
    val sorted = elems.sorted
    for (i <- sorted.indices)
      assert(tree(i) == sorted(i))
  }
}
