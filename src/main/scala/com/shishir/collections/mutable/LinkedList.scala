package com.shishir.collections.mutable

import scala.collection.mutable

/**
  * LinkedList written in a Java style
  * I felt gross writing this
  * @tparam A Type of list elements
  */
class LinkedList[A](elems: A*) extends mutable.Buffer[A] {
  private case class Node(var data: A, var next: Option[Node])

  private var start: Option[Node] = None
  private var end: Option[Node] = None
  private var _length = 0

  insertAll(0, elems)

  def iterator: Iterator[A] = Iterator
    .iterate(start)(_.flatMap(_.next))
    .takeWhile(_.isDefined)
    .map(_.get.data)

  def update(idx: Int, elem: A): Unit = {
    if (idx < 0 || idx > length) throw new IndexOutOfBoundsException
    if (length == 0) {
      start = Some(Node(elem, None))
      end = start
      _length += 1
      return
    }
    val head = start.get
    val tail = end.get
    if (idx == length) {
      tail.next = Some(Node(elem, None))
      end = tail.next
    } else if (idx == 0) {
      start = Some(Node(elem, start))
    } else {
      var currNode = head
      var currInd = 1
      while (currInd != idx) {
        currInd += 1
        currNode = head.next.get
      }
      currNode.next = Some(Node(elem, currNode.next))
    }
    _length += 1
  }

  def length: Int = _length

  def apply(idx: Int): A = {
    if (idx < 0 || idx >= length) throw new IndexOutOfBoundsException
    var curr = start
    var currInd = 0
    while (curr.isDefined) {
      if (idx == currInd)
        return curr.get.data
      currInd += 1
      curr = curr.flatMap(_.next)
    }
    throw new IndexOutOfBoundsException
  }

  override def +=(elem: A): LinkedList.this.type = {
    update(length, elem)
    this
  }

  def clear(): Unit = {
    start = None
    end = None
    _length = 0
  }

  def +=:(elem: A): LinkedList.this.type = {
    update(0, elem)
    this
  }

  def insertAll(n: Int, elems: Traversable[A]): Unit = {
    if (n < 0 || n > length) throw new IndexOutOfBoundsException
    if (elems.isEmpty) return
    _length += elems.size
    val startInsertion = Some(Node(elems.head, None))
    def createChain(node: Option[Node], elements: Traversable[A]): Node = {
      if (elements.isEmpty) node.get
      else {
        node.get.next = Some(Node(elements.head, None))
        createChain(node.get.next, elements.tail)
      }
    }
    val lastNode = Some(createChain(startInsertion, elems.tail))
    if (n == 0) {
      start = startInsertion
    } else {
      var currNode: Option[Node] = startInsertion
      var currInd = 1
      while (currInd != n) {
        currInd += 1
        currNode = startInsertion.flatMap(_.next)
      }
      lastNode.get.next = currNode.get.next
      currNode.get.next = startInsertion
    }
    if (n > length - elems.size) {
      end = lastNode
    }
  }

  def remove(n: Int): A = {
    if (n < 0 || n >= length) throw new IndexOutOfBoundsException
    if (length == 1) {
      val removed = start.get.data
      start = None
      end = None
      _length -= 1
      return removed
    }
    if (n == 0) {
      val removed = start.get.data
      start = start.flatMap(_.next)
      _length -= 1
      return removed
    }
    var currNode = start
    var currInd = 1
    while (currInd != n) {
      currInd += 1
      currNode = currNode.flatMap(_.next)
    }
    val removed = currNode.flatMap(_.next)
    _length -= 1
    if (n == length) {
      end = currNode
    }
    currNode.get.next = removed.flatMap(_.next)
    removed.get.data
  }
}
