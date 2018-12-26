package com.shishir.collections

import scala.annotation.tailrec

object List {
  def apply(): List[Nothing] = Nil
  def apply[A](elems: A*): List[A] = (elems :\ (Nil: List[A]))(_ :: _)
  def unapply[A](arg: List[A]): Option[(A, List[A])] = arg match {
    case Node(x, tail) => Some(x, tail)
    case Nil => None
  }
  def unapplySeq[A](list: List[A]): Option[Seq[A]] = Some(list)
}

abstract sealed class List[+A] extends Seq[A] {
  def ::[B >: A](elem: B): List[B] = Node(elem, this)

  def iterator: Iterator[A] = {
    def toStream(list: List[A]): Stream[A] = list match {
      case Node(value, rest) => value #:: toStream(rest)
      case Nil => Stream.empty
    }
    toStream(this).iterator
  }

  @tailrec
  final def apply(idx: Int): A = {
    if (idx < 0) throw new IndexOutOfBoundsException(idx)
    else this match {
      case Nil => throw new IndexOutOfBoundsException
      case Node(x, rest) =>
        if (idx == 0) x
        else rest.apply(idx - 1)
    }
  }

  def length: Int = {
    @tailrec
    def length[C](list: List[C], accum: Int): Int = list match {
      case Nil => accum
      case Node(_, rest) => length(rest, accum + 1)
    }
    length(this, 0)
  }

  override def reverse: List[A] =
    ((Nil: List[A]) /: this)((list, elem) => elem :: list)

  override def head: A = this match {
    case Node(x, _) => x
    case Nil => throw new NoSuchElementException("head of empty list")
  }

  override def tail: List[A] = this match {
    case Node(_, rest) => rest
    case Nil => throw new UnsupportedOperationException("tail of empty list")
  }

  override def isEmpty: Boolean = this match {
    case Nil => true
    case _ => false
  }
}

private case class Node[A](value: A, rest: List[A]) extends List[A]
case object Nil extends List[Nothing]