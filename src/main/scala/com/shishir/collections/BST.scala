package com.shishir.collections

object BST {
  def apply(): BST[Nothing] = Leaf
  def apply[A](elems: A*)(implicit ordering: Ordering[A]): BST[A] =
    ((Leaf: BST[A]) /: elems)(_ + _)
}

abstract sealed class BST[+A] {
  def +[B >: A](elem: B)(implicit ordering: Ordering[B]): BST[B] = this match {
    case Leaf => Parent(elem, Leaf, Leaf)
    case Parent(x, l, r) =>
      if (ordering.gt(elem, x)) Parent(x, l, (r: BST[B]) + elem)
      else if (ordering.lt(elem, x)) Parent(x, (l: BST[B]) + elem, r)
      else this
  }

  def add[B >: A](elem: B)(implicit ordering: Ordering[B]): BST[B] = this + elem

  def -[B >: A](elem: B)(implicit ordering: Ordering[B]): BST[B] = this match {
    case Leaf => Leaf
    case Parent(x, l, r) =>
      if (ordering.lt(elem, x)) Parent(x, l - elem, r)
      else if (ordering.gt(elem, x)) Parent(x, l, r - elem)
      else (l: BST[B]).addTree(r)
  }

  private def addTree[C >: A](subtree: BST[C])(implicit ordering: Ordering[C]): BST[C] = (this, subtree) match {
    case (Leaf, _) => subtree
    case (_, Leaf) => this
    case (Parent(x, l, r), Parent(x2, l2, r2)) =>
      if (ordering.gt(x2, x)) Parent(x, l, r.addTree(subtree))
      else if (ordering.lt(x2, x)) Parent(x, l.addTree(subtree), r)
      else Parent(x, l.addTree(l2), r.addTree(r2))
  }

  def remove[B >: A](elem: B)(implicit ordering: Ordering[B]): BST[B] = this - elem
  def contains[B >: A](elem: B): Boolean = this match {
    case Leaf => false
    case Parent(x, l, r) => x == elem || l.contains(elem) || r.contains(elem)
  }

  def isEmpty: Boolean = this match {
    case Leaf => true
    case _ => false
  }

  def iterator: Iterator[A] = this match {
    case Leaf => Iterator()
    case Parent(x, l, r) => l.iterator ++ Iterator(x) ++ r.iterator
  }

  def size: Int = this match {
    case Leaf => 0
    case Parent(_, l, r) => 1 + l.size + r.size
  }

  def apply(idx: Int): A = this match {
    case Leaf => throw new IndexOutOfBoundsException
    case Parent(x, l, r) =>
      val leftSize = l.size
      if (idx == leftSize) x
      else if (idx < leftSize) l(idx)
      else r(idx - 1 - leftSize)
  }
}
private case class Parent[A](value: A, left: BST[A], right: BST[A]) extends BST[A]
private case object Leaf extends BST[Nothing]
