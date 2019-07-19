// Mateusz Najda

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.io.Source
import scala.util.{Failure, Success}

object lista11 {

  def pairFut[A, B] (fut1: Future[A], fut2: Future[B]): Future[(A, B)] =
    fut1 zip fut2

  def pairFutFor[A, B] (fut1: Future[A], fut2: Future[B]): Future[(A, B)] =
    for {
      a <- fut1
      b <- fut2
    } yield (a, b)

  implicit class FutureOps[T](val self: Future[T]) {

    def existsPromise(p: T => Boolean): Future[Boolean] = {
      val promise = Promise[Boolean]
      self onComplete {
        case Success(output) => promise.success(p(output))
        case Failure(_) => promise.success(false)
      }
      promise.future
    }

    def exists(p: T => Boolean): Future[Boolean] =
       self map p recover{ case _ => false }
  }

  def main(args: Array[String]): Unit = {

    val pair1 = pairFut(Future{1+1}, Future{2+2})
    val pair1Fail = pairFut(Future{1+1}, Future{1/0})
    val pair2 = pairFutFor(Future{1+1}, Future{2+2})
    val pair2Fail = pairFutFor(Future{1+1}, Future{1/0})

    Thread.sleep(500)

    println(pair1.value)
    println(pair1Fail.value)

    println(pair2.value)
    println(pair2Fail.value)

    println()

    val futurePromiseSuccess = Future{1+1}.existsPromise( _ == 2)
    val futurePromiseFail = Future{2+1}.existsPromise( _ == 2)
    val futurePromiseException = Future{1/0}.existsPromise(_ == 2)

    val futureSuccess = Future{1+1}.exists( _ == 2)
    val futureFail = Future{2+1}.exists(_ == 2)
    val futureException = Future{1/0}.exists(_ == 2)

    Thread.sleep(500)

    println(futurePromiseSuccess.value)
    println(futurePromiseFail.value)
    println(futurePromiseException.value)

    println(futureSuccess.value)
    println(futureFail.value)
    println(futureException.value)
  }
}

object WordCount {
  def main(args: Array[String]) {
    val path = "test/"
    val promiseOfFinalResult = Promise[Seq[(String, Int)]]

    promiseOfFinalResult.future onComplete {
      case Success(result) => result foreach println
      case Failure(t) => t.printStackTrace()
    }

    val futureProcessFiles = Promise[Seq[(String, Int)]]
    futureProcessFiles.future onComplete {
      case Success(success) => promiseOfFinalResult.success(success.sortWith((lhs, rhs) => { lhs._2 < rhs._2 }))
      case Failure(failure) => failure.printStackTrace()
    }

    scanFiles(path) onComplete {
      case Success(files) =>
        processFiles(files) onComplete {
          case Success(success) => futureProcessFiles.success(success)
          case Failure(failure) => failure.printStackTrace()
        }
      case Failure(failure) => failure.printStackTrace()
    }

    Thread.sleep(5000)
  }

  private def processFiles(fileNames: Seq[String]): Future[Seq[(String, Int)]] =
    Future.sequence(fileNames.map(filename => processFile(filename)))

  private def processFile(fileName: String): Future[(String, Int)] = {
    val file = Source.fromFile(fileName)
    Future{ (fileName, file.mkString.split(" ").length) }
  }

  private def scanFiles(docRoot: String): Future[Seq[String]] =
    Future { new java.io.File(docRoot).list.map(docRoot + _) }
}
