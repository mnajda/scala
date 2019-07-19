// Mateusz Najda

def take[A](n: Int, xs: List[A]): List[A] = xs match {
  case h :: t => if (n > 0) h :: take(n - 1, t) else Nil
  case Nil => Nil
}

take(2, List(1,2,3,5,6)) == List(1,2)
take(-2, List(1,2,3,5,6)) == Nil
take(8, List(1,2,3,5,6)) == List(1,2,3,5,6)
take(8, List()) == Nil
take(8, Nil) == Nil
take(0, List(1,2,3)) == Nil

def drop[A](n: Int, xs: List[A]): List[A] = xs match {
  case _ :: t => if (n > 0) drop(n - 1, t) else xs
  case Nil => Nil
}

drop(2, List(1,2,3,5,6)) == List(3,5,6)
drop(-2, List(1,2,3,5,6)) == List(1,2,3,5,6)
drop(8, List(1,2,3,5,6)) == Nil
drop(2, List()) == Nil
drop(2, Nil) == Nil
drop(0, List(1,2,3)) == List(1,2,3)

def reverse[A](xs: List[A]): List[A] = {
  def acc(in: List[A], out: List[A]): List[A] = {
    in match {
      case h :: t => acc(t, h :: out)
      case Nil => out
    }
  }

  acc(xs, List())
}

reverse(List("Ala", "ma", "kota")) == List("kota", "ma", "Ala")
reverse(Nil) == Nil
reverse(List()) == Nil

def replicate(xs: List[Int]): List[Int] = {
  def fill(acc: List[Int], count: Int, value: Int): List[Int] = {
    if (count > 0) fill(value :: acc, count - 1, value) else acc
  }

  def repl(acc: List[Int], in: List[Int]): List[Int] = in match {
    case h :: t => repl(acc ::: fill(List(), h, h), t)
    case Nil => acc
  }

  repl(List(), xs)
}

replicate(List(1,0,4,-2,3)) == List(1, 4, 4, 4, 4, 3, 3, 3)
replicate(List(1)) == List(1)
replicate(List(2)) == List(2, 2)
replicate(List()) == Nil
replicate(Nil) == Nil

import scala.math.abs
import scala.math.pow

def root3(a: Double): Double = {
  val epsilon = 10e-15
  val initialAbsolute = abs(a)
  val bound = epsilon * initialAbsolute

  def recursive(current: Double): Double = {
    val isFinished = abs(pow(current, 3.0) - a) <= bound

    if (isFinished) current
    else {
      val currentSquared = current * current
      val next = current + (((a / currentSquared) - current) / 3.0)
      recursive(next)
    }
  }

  recursive(if (a > 1.0) a / 3.0 else a)
}

root3(0.0) == 0.0
root3(1.0) == 1.0
root3(8.0) == 2.0
root3(-8.0) == -2.0
