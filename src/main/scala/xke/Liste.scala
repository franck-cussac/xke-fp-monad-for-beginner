package xke

sealed trait Liste[+A] {
  def head: A
  def tail: Liste[A]
  def flatMap[B](f: A => Liste[B]): Liste[B] = {
    if (this == Empty) Empty
    else f(head).union(tail.flatMap(f))
  }

  def union[B >: A](that: Liste[B]): Liste[B] = this match {
    case ListeA(head, tail) => ListeA(head, tail.union(that))
    case Empty => that
  }
}

case class ListeA[A](head: A, tail: Liste[A]) extends Liste[A]

case object Empty extends Liste[Nothing] {
  override def head = throw new NoSuchElementException("call head on xke.Empty list")

  override def tail = throw new NoSuchElementException("call tail on xke.Empty list")
}

object Liste {
  def apply[A](elem: A*): Liste[A] = if (elem.isEmpty) Empty else ListeA(elem.head, Liste(elem.tail:_*))
}
