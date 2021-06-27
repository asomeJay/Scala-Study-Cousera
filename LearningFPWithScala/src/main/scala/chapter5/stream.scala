package chapter5

import chapter5.Stream._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer


sealed trait Stream[+A] {
  def toList  : List[A] = {
    val buf = new ListBuffer[A]
    @tailrec
    def go(s:Stream[A]):List[A] = s match {
      case Cons(h,t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }

    go(this)
  }

  def take(n: Int):Stream[A] = {
    this match {
      case Cons(h,t) => if ( n > 1) cons(h(), t().take(n-1))
      case Cons(h, _)=>if (n == 0) cons(h(), empty)
      case _ => empty
    }
  }

  def drop(n : Int):Stream[A] = {
    this match {
      case Cons(_,t) => if (n > 1) t().drop(n-1)
      case _ => empty
    }
  }

  def takeWhile(p:A=>Boolean): Stream[A] = this match {
    case Cons(h,t) => if(p(h())) cons(h, t().takeWhile(p))
    case _ => empty
  }

  def exists(p: A=>Boolean) :Boolean = this match {
    case Cons(h,t) => p(h()) || t().exists(p)
    case _ => false
  }

  def forAll(p: A=>Boolean) : Boolean = this match {
    case Cons(t,h) => if( p(t())) h().forAll(p) else false
    case _ => true
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }
  def takeWhile2(p:A=>Boolean): Stream[A] = foldRight(empty[A])((h,t) => if(p(h)) cons(h,t) else empty)
  def constant[A](a:A):Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }
  def zipAll[B](s2:Stream[B]): Stream[(Option[A], Option[B])] = zipAllWith(s2)((_,_))

  def zipAllWith[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C) = {
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h1, t1), Empty) => Some(f(Some(h1()),  Option.empty[B]) -> (t1(), empty[B]))
      case (Empty, Cons(h1, t1)) => Some(f(Option.empty[A], Some(h1())) -> (empty[A], t1()))
      case (Cons(h1,t1), Cons(h2,t2)) => Some(f(Some(h1()), Some(h2())) -> (t1(), t2()))
    }
  }

  def startsWith[A](s:Stream[A]): Boolean = zipAll(s).takeWhile(_._2.isDefined).forAll {
    case (h, h2) => h == h2
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case s => Some((s, s drop 1))
  } append empty

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => cons(h,t))
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(()=> head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as:A*): Stream[A] =
    if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def fibs(): Stream[Int] = {
    def go(first : Int, second:Int ):Stream[Int] = {
      cons(first, go(second, first + second))
    }
    go(0,1)
  }

  def unfold[A,S](z:S)(f:S=>Option[(A,S)]):Stream[A] = f(z) match {
    case Some((h,t)) =>cons(h,unfold(t)(f))
    case None => empty
  }

  def fib2(): Stream[Int] = unfold((0,1)){case (f1,s1)=>Some((f1, (s1, f1 + s1)))} // ?}
  def fromViaUnfold(n: Int) =
    unfold(n)(n => Some((n,n+1)))

  def constantViaUnfold[A](a: A) =
    unfold(a)(_ => Some((a,a)))

  def onesViaUnfold() = unfold(1)(_ => Some((1,1)))







}