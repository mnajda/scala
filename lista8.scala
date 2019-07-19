// Mateusz Najda

import scala.reflect.ClassTag

class FullException(msg: String) extends Exception(msg)
abstract class MyQueue[E] {
  @throws[FullException]
  def enqueue(x: E): Unit
  def dequeue(): Unit
  @throws[NoSuchElementException]
  def first: E
  def isEmpty: Boolean
  def isFull: Boolean
}

class QueueMut[E: ClassTag](val capacity: Int = 1000) extends MyQueue[E] {
  private val cap: Int = capacity + 1
  private val queue: Array[E] = new Array[E](cap)
  private var f: Int = 0
  private var r: Int = 0

  override def enqueue(x: E): Unit =
    if (isFull) throw new FullException("queue is full")
    else {
      queue(r) = x
      r = (r + 1) % cap
    }

  override def dequeue(): Unit =
    if (isEmpty) {}
    else f = (f + 1) % cap

  override def first: E =
    if (isEmpty) throw new NoSuchElementException("queue is empty")
    else queue(f)

  override def isEmpty: Boolean = f == r

  override def isFull: Boolean = f == (r + 1) % cap
}

object QueueMut {
  def apply[E: ClassTag](xs: E*): QueueMut[E] = {
    val queue: QueueMut[E] = new QueueMut[E]()
    for (x <- xs)
      queue.enqueue(x)
    queue
  }

  def empty[E: ClassTag](capacity: Int = 1000): QueueMut[E] = new QueueMut[E](capacity)
}

object lista8 {
  def main(args: Array[String]): Unit = {
    val q: QueueMut[Int] = QueueMut(1,2,3)
    val q2: QueueMut[Int] = QueueMut()
    val q3: QueueMut[Int] = QueueMut.empty(3)

    val queue: QueueMut[Int] = new QueueMut[Int](3)
    println(queue.isEmpty == true)
    queue.enqueue(1)
    println(queue.first == 1)

    queue.enqueue(2)
    println(queue.first == 1)

    queue.enqueue(3)
    println(queue.first == 1)

    try {
      queue.enqueue(4)
    }
    catch {
      case e: FullException => println(e.getMessage)
    }

    println(queue.isFull == true)

    queue.dequeue()
    println(queue.first == 2)

    queue.dequeue()
    println(queue.first == 3)

    queue.dequeue()
    try {
      queue.first
    }
    catch {
      case e: NoSuchElementException => println(e.getMessage)
    }

    queue.enqueue(4)
    println(queue.first == 4)
  }
}
