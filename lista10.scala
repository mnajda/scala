// Mateusz Najda

import scala.concurrent.ExecutionContext
import scala.util.Random
import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue, Semaphore}

class Producer(name: String, buf: BlockingQueue[Int]) extends Thread(name) {
  override def run(): Unit =
    for (i <- 1 to 10) { println(s"$getName producing $i"); buf.put(i) }
}

class Consumer(name: String, buf: BlockingQueue[Int]) extends Thread(name) {
  override def run(): Unit =
    for (_ <- 1 to 10) println(s"$getName consumed ${buf.take}")
}

object prodCons {

  def zad1a(): Unit = {
    val buf: BlockingQueue[Int] = new ArrayBlockingQueue[Int](5)
    new Producer("Producer", buf).start()
    new Consumer("Consumer", buf).start()
  }

  def zad1b(): Unit = {
    val buf: BlockingQueue[Int] = new ArrayBlockingQueue[Int](5)
    val producersCount: Int = 2
    val consumersCount: Int = 3

    println(s"Producers: $producersCount, Consumers: $consumersCount")
    for (i <- 1 to producersCount) new Producer(s"Producer$i", buf).start()
    for (i <- 1 to consumersCount) new Consumer(s"Consumer$i", buf).start()
  }

  def main(args: Array[String]): Unit = {
    zad1a()
    zad1b()
  }
}

object executionContext {
  def produce(buf: BlockingQueue[Int]): Unit = {
    for (i <- 1 to 10) { println(s"${Thread.currentThread().getName} producing $i"); buf.put(i) }
  }

  def consume(buf: BlockingQueue[Int]): Unit = {
    for (_ <- 1 to 10) println(s"${Thread.currentThread().getName} consumed ${buf.take}")
  }

  def main(args: Array[String]): Unit = {
    val buf: BlockingQueue[Int] = new ArrayBlockingQueue[Int](5)
    val producersCount: Int = 2
    val consumersCount: Int = 3
    val context = ExecutionContext.global

    for (_ <- 1 to producersCount) context.execute(() => produce(buf))
    for (_ <- 1 to consumersCount) context.execute(() => consume(buf))

    Thread.sleep(500)
  }
}

class Table(seatCount: Int = 5) {
  var forks: Array[Semaphore] = Array.fill(seatCount)(new Semaphore(1))
  var butler: Semaphore = new Semaphore(seatCount - 1)

  def eat(philosopherId: Int): Unit = {
    println(s"Philosopher $philosopherId stops thinking...")

    val firstFork: Int = philosopherId
    val secondFork: Int = (philosopherId + 1) % seatCount

    butler.acquire()
    forks(firstFork).acquire()
    forks(secondFork).acquire()

    println(s"Philosopher $philosopherId started eating...")
    Thread.sleep(200 + Random.nextInt(300))

    forks(firstFork).release()
    forks(secondFork).release()
    butler.release()

    println(s"Philosopher $philosopherId stops eating...")
  }
}

object lista10 {
  def main(args: Array[String]): Unit = {
    val philosophers: Int = 5
    val table: Table = new Table(philosophers)

    for (i <- 0 until philosophers) {
      new Thread(() => {
        while(true) {
          println(s"Philosopher $i starts thinking...")
          Thread.sleep(500 + Random.nextInt(500))
          table.eat(i)
        }
      }).start()
    }
  }
}
