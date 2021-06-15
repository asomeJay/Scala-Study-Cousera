package chapter3

import scala.::
import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match{
    case Nil => 0
    case Cons(x, xs) => x+ sum(xs)
  }

  def prouct(ds:List[Double]):Double = ds match{
    case Nil => 1.0
    case Cons(x, xs) => x * prouct(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail:_*))

  def append[A](a1:List[A], a2:List[A]):List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def foldRight[A,B] (as: List[A], z:B)(f:(A,B) => B):B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns:List[Int]):Int = foldRight(ns, 0)(_+_)
  def product2(ns:List[Int]):Int = foldRight(ns, 0)(_*_)

  // 3-1 : 정답은 '3' 세번째 case에 매칭된다. 만약 저 case가 없다면 네 번째 case에 매칭된다.
  val x = List(1,2,3,4,5) match{
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4,_)))) => x + y
    case Cons(h,t) => h+sum(t)
    case _ => 101
  }

  // 3-2
  def tail[A](l : List[A]):List[A] = l match {
    case Nil => Nil
    case Cons(_,t) => t
  }

  // 3-3
  def setHead[A](l:List[A], alternative:A):List[A] = l match{
    case Nil => Nil
    case Cons(_,t) => Cons(alternative, t)
  }

  // 3-4
  @tailrec
  def drop[A](l:List[A], n:Int):List[A] =
    if (n > 0) drop(tail(l), n-1)
    else l

  // 3-5
  @tailrec
  def dropWhile[A](l:List[A], f: A=>Boolean):List[A] = l match {
    case Nil => Nil
    case Cons(h,t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  // 3-6
  def init[A](l:List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h,t)=>Cons(h, init(t))
  }

  // 3-7 왜 멈춰...? 종료조건이 없는데...? 이게 왜 심오한 문제야...?
  // 3-8 Cons(1, Cons(2, Cons(3, Nil))) 처럼 동작함. 생성자랑 반대로 동작하는듯 결과는 같긴한데.

   // 3 - 9
   def length[A](as:List[A]):Int = foldRight(as, 0)((_, x:Int)=> x + 1)

  // 3- 10
  //  var a = List(0)
  //  for (num <- 0 to 10000000) a = append(a, List(num))
  //  println(length(a))
  def foldLeft[A,B](as:List[A], z:B) (f:(B,A)=>B):B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t,f(z, h))(f)
  }

  // 3-12
  def reverse[A](l:List[A]):List[A] = foldLeft(l, List[A]())((h, t)=>Cons(t, h))

  // 3-13
  def foldRight2[A,B] (as: List[A], z:B)(f:(A,B) => B):B = foldLeft(as, z)((a,b)=>f(b,a))

  // 3-14
  def append2[A] (l:List[A], m:List[A]) : List[A] = foldLeft(l, m)((m, l) =>Cons(l,m))

  // 3-15
  def connect[A](l:List[List[A]]):List[A] = foldLeft(l, Nil:List[A])((a:List[A], b:List[A]) => append(a,b))

  // 3-16
  def addOne(l:List[Int]):List[Int] = foldRight(l, Nil:List[Int])((h:Int,list:List[Int])=>Cons(h+1, list))

  // 3-17
  def doubleToString(l:List[Double]):List[String] = foldRight(l, Nil:List[String])((h:Double, t:List[String])=>Cons(h.toString, t))

  // 3-18
  def map[A,B](as:List[A])(f:A=>B):List[B] = foldRight(as, Nil:List[B])((h:A, t:List[B])=>Cons(f(h), t))

  // 3-19
  def filter[A](as:List[A])(f: A=>Boolean):List[A] = as match {
    case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
    case Cons(x, xs) if !f(x) => filter(xs)(f)
    case Nil => Nil
  }

  // 3-20
  def flatMap[A,B](as:List[A])(f:A=>List[B]):List[B] = foldRight(as,Nil:List[B])((h, t:List[B]) => append(f(h), t))

  // 3-21
  def filterUsingFlatMap[A](as:List[A])(f: A=>Boolean):List[A] =
    flatMap(as)(x => if (f(x)) List(x) else Nil:List[A])

  // 3-22
  def indexSum(l:List[Int], m:List[Int]):List[Int] = (l, m) match {
    case (Cons(lh:Int, lt: List[Int]), Cons(mh:Int, mt: List[Int])) => Cons(lh + mh, indexSum(lt, mt))
    case _ => Nil
  }

  // 3-23
  def zipWidth[A,B,C](l:List[A], m:List[B])(f: (A,B) => C):List[C] = (l,m) match {
    case (Cons(lh, lt), Cons(mh,mt)) => Cons(f(lh, mh), zipWidth(lt, mt)(f))
    case _ => Nil
  }

  def main(args: Array[String]): Unit = {
    println(indexSum(List(1,2,3), List(3,4,5)))
  }

}
