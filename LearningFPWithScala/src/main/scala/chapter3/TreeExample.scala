package chapter3

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A], right:Tree[A]) extends Tree[A]

object TreeExample {
  // 3-25
  def size[A](tree:Tree[A]):Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  // 3-26
  def maximum(tree:Tree[Int], x:Int):Int = tree match {
    case Leaf(v) => v max x
    case Branch(l, r) => maximum(l, x) max  maximum(r,x)
  }

  // 3-27
  def depth[A](tree:Tree[A], d:Int):Int = tree match {
    case Leaf(_) => d
    case Branch(l, r) => depth(l, d+1) max depth(r, d+1)
  }

  // 3-27-2
  def depth[A](tree:Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + depth(l) max depth(r)
  }

  // 3-28
  def map[A,B](tree:Tree[A], f:A=>B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l, f), map(r, f))
  }


  def main(args: Array[String]): Unit = {
    def falling(i:Int): Int = {
      val y: Int = throw new Exception("THREAD")
      try {
        val x = 42 + 5
        x + y
      } catch {
        case e: Exception => 43
      }
    }

    falling(3)
    println("?")
  }
}
