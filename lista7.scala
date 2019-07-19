// Mateusz Najda

class MyQueue[+A] private (private val begin: List[A], private val end: List[A]) {
  def this() = this(List(), List())

  def enqueue[B >: A](value: B): MyQueue[B] = {
    if (begin.nonEmpty) new MyQueue(begin, value :: end)
    else new MyQueue(List(value), end)
  }

  def first: A = begin.head

  def firstOption: Option[A] = begin match {
    case h :: _ => Some(h)
    case Nil => None
  }

  def isEmpty: Boolean = begin.isEmpty

  def dequeue: MyQueue[A] = begin match {
    case _ :: second :: tail => new MyQueue(second :: tail, end)
    case _ :: Nil => new MyQueue(end.reverse, List())
    case Nil => MyQueue.empty
  }
}

object MyQueue {
  def empty = new MyQueue
  def apply[A](in: A*): MyQueue[A] = new MyQueue(in.toList, List())
}

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

object Lista7 {
  def main(args: Array[String]): Unit = {
    val q = new MyQueue
    val q2 = MyQueue.empty
    val q3 = MyQueue()
    val q4 = MyQueue("a", "b", "c")

    var queue = MyQueue(1, 2, 3)

    println(queue.first == 1)

    queue = queue.dequeue
    println(queue.first == 2)

    queue = queue.dequeue
    println(queue.firstOption == Some(3))

    queue = queue.dequeue
    println(queue.dequeue.isEmpty == true)
    println(queue.firstOption == None)

    try {
      queue.first
    }
    catch {
      case e: NoSuchElementException => println(e.getMessage)
    }

    queue = queue.enqueue(4)
    println(queue.first == 4)

    queue = queue.enqueue(5)
    println(queue.first == 4)

    def breadthBT[A](tree: BT[A]): List[A] = {
      def bfs(queue: MyQueue[BT[A]]): List[A] = queue.firstOption match {
        case Some(Node(value, left, right)) => value :: bfs(queue.dequeue.enqueue(left).enqueue(right))
        case Some(Empty) => bfs(queue.dequeue)
        case None => Nil
      }

      bfs(MyQueue(tree))
    }

    val t = Node(1, Node(2, Empty, Node(3, Empty, Empty)), Empty)
    val tt = Node(1,
      Node(2,
        Node(4,
          Empty,
          Empty
        ),
        Empty
      ),
      Node(3,
        Node(5,
          Empty,
          Node(6,
            Empty,
            Empty
          )
        ),
        Empty
      )
    )

    println(breadthBT(Empty) == List())
    println(breadthBT(t) == List(1,2,3))
    println(breadthBT(tt) == List(1,2,3,4,5,6))
  }
}
