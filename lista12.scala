// Mateusz Najda

import akka.actor._
import scala.util.Random

class Server(upperBound: Int) extends Actor {
  private val valueToGuess: Int = Random.nextInt(upperBound)

  println(s"Guess my number from the interval [0..$upperBound], correct answer is $valueToGuess")

  override def receive: PartialFunction[Any, Unit] = {
    case Server.M(guess) =>
      if (guess == valueToGuess) sender ! Client.R("eq")
      if (guess > valueToGuess) sender ! Client.R("g")
      if (guess < valueToGuess) sender ! Client.R("l")
    case message => throw new Exception(s"Server received invalid message: $message")
  }
}

object Server {
  case class M(msg: Int)
}

class Client(name: String, server: ActorRef, upperBound: Int) extends Actor {
  private var begin: Int = 0
  private var end: Int = upperBound
  private var guess: Int = Random.nextInt(upperBound)

  def makeGuess(begin: Int, end: Int): Int = (begin + end) / 2

  println(s"$name starting")

  override def receive: PartialFunction[Any, Unit] = {
    case Client.R(response) => response match {
      case "eq" =>
        println(s"$name: I guessed it! $guess")
        context.system.terminate
      case "g"  =>
        end = guess
        guess = makeGuess(begin, end)
        println(s"$name Response: too big. I'm trying: $guess")
        server ! Server.M(guess)
      case "l" =>
        begin = guess
        guess = makeGuess(begin, end)
        println(s"$name Response: too small. I'm trying: $guess")
        server ! Server.M(guess)
    }
    case Client.Start =>
      println(s"$name trying: $guess")
      server ! Server.M(guess)
    case other => throw new Exception(s"$name: Received invalid message: $other")
  }
}

object Client {
  case class R(response: String)
  case object Start
}

object lista12 {
  private val upperBound: Int = 100
  private val numberOfClients: Int = 2

  def main(args: Array[String]): Unit = {
    val ourSystem = ActorSystem("MySystem")
    val server: ActorRef = ourSystem.actorOf(Props(classOf[Server], upperBound))

    for (i <- 1 to numberOfClients)
      ourSystem.actorOf(Props(classOf[Client], s"Client$i", server, upperBound)) ! Client.Start

    Thread.sleep(1000)
  }
}
