// Mateusz Najda

import java.util.concurrent.Semaphore

object Zad1 extends App {
  var counter = 0 // counter variable

  def readWriteCounter(): Unit = {
    val incrementedCounter = counter + 1 // reading counter
    counter = incrementedCounter // writing to counter
  }

  val p = new Thread(() => for(_ <- 0 until 200000) readWriteCounter())
  val q = new Thread(() => for(_ <- 0 until 200000) readWriteCounter())
  val startTime = System.nanoTime
  p.start(); q.start()
  p.join(); q.join()
  val estimatedTime = (System.nanoTime - startTime) / 1000000
  println(s"The value of counter = $counter")
  println(s"Estimated time = ${estimatedTime}ms, Available processors = ${Runtime.getRuntime.availableProcessors}")
}

object Zad1b extends App {
  var counter = 0 // counter variable

  def readWriteCounter(): Unit = {
    this.synchronized {
      val incrementedCounter = counter + 1 // reading counter
      counter = incrementedCounter // writing to counter
    }
  }

  val p = new Thread(() => for(_ <- 0 until 200000) readWriteCounter())
  val q = new Thread(() => for(_ <- 0 until 200000) readWriteCounter())
  val startTime = System.nanoTime
  p.start(); q.start()
  p.join(); q.join()
  val estimatedTime = (System.nanoTime - startTime)/1000000
  println(s"The value of counter = $counter")
  println(s"Estimated time = ${estimatedTime}ms, Available processors = ${Runtime.getRuntime.availableProcessors}")
}

object Zad1c extends App {
  var counter = 0 // counter variable

  var semaphore = new Semaphore(1)

  def readWriteCounter(): Unit = {
    semaphore.acquire()

    val incrementedCounter = counter + 1 // reading counter
    counter = incrementedCounter // writing to counter

    semaphore.release()
  }

  val p = new Thread(() => for(_ <- 0 until 200000) readWriteCounter())
  val q = new Thread(() => for(_ <- 0 until 200000) readWriteCounter())
  val startTime = System.nanoTime
  p.start(); q.start()
  p.join(); q.join()
  val estimatedTime = (System.nanoTime - startTime)/1000000
  println(s"The value of counter = $counter")
  println(s"Estimated time = ${estimatedTime}ms, Available processors = ${Runtime.getRuntime.availableProcessors}")
}

object lista9 {

  def parallel[A, B](block1: => A, block2: => B): (A, B) = {

    var a: Option[A] = None
    var b: Option[B] = None

    val thread1 = new Thread( () => a = Some(block1) )
    val thread2 = new Thread( () => b = Some(block2) )

    thread1.start(); thread2.start()
    thread1.join(); thread2.join()

    (a.get, b.get)
  }

  def periodically(duration: Long, times: Int)(block: => Unit): Unit = {
    val thread = new Thread( () => {
      for (_  <- 0 to times) {
        block
        Thread.sleep(duration)
      }
    })

    thread.setDaemon(true)
    thread.start()
  }

  def main(args: Array[String]): Unit = {
    println(parallel("a"+1, "b"+2))
    println(parallel(Thread.currentThread.getName, Thread.currentThread.getName))
    println(parallel({1+1}, {2+2}))

    periodically(1000, 5){print("y ")}
    periodically(1000, 25){print("x ")}
    Thread.sleep(10000)
    println("Done sleeping")
  }
}
