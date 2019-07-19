// Mateusz Najda

def whileLoop(predicate: => Boolean)(expression: => Unit): Unit =
  if (predicate) {
    expression
    whileLoop(predicate)(expression)
  }

var count = 0
whileLoop(count < 5) {
  println(count)
  count += 1
}

def lrepeat[A](k: Int, stream: Stream[A]): Stream[A] = {
  def fill(acc: Stream[A], count: Int, value: A): Stream[A] = {
    if (count > 0) fill(value #:: acc, count - 1, value) else acc
  }

  stream match {
    case h #:: t => fill(Stream.Empty, k, h) #::: lrepeat(k, t)
    case Stream.Empty => Stream.Empty
  }
}

lrepeat(2, Stream.Empty).toList == List()
lrepeat(1, 1  #:: Stream.Empty).toList == List(1)
lrepeat(3, 1  #:: 2  #:: Stream.Empty).toList == List(1, 1, 1, 2, 2, 2)
(lrepeat(3, Stream.from(1)) take 12).toList == List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)

sealed trait lBT[+A]
case object LEmpty extends lBT[Nothing]
case class LNode[+A](elem: A, left: () => lBT[A], right: () => lBT[A]) extends lBT[A]

def lBreadth[A](ltree: lBT[A]): Stream[A] = {
  def bfs(queue: List[lBT[A]]): Stream[A] = queue match {
    case LNode(value, left, right) :: t => value #:: bfs(t ::: List(left(), right()))
    case LEmpty :: t => bfs(t)
    case Nil => Stream.Empty
  }

  bfs(List(ltree))
}

def lTree(n: Int): lBT[Int] = {
  LNode(n, () => lTree(2 * n), () => lTree((2 * n) + 1))
}

lBreadth(LEmpty).take(10) == Stream.Empty
lBreadth(LNode(1, () => LNode(2, () => LEmpty, () => LEmpty), () => LNode(3, () => LEmpty, () => LEmpty))).toList == List(1,2,3)
lBreadth(lTree(1)).take(10).toList == List(1,2,3,4,5,6,7,8,9,10)
