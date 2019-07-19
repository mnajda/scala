// Mateusz Najda

val suma: List[Double] => Double = xs =>
  if (xs == Nil) 0.0
  else xs.head + suma(xs.tail)

suma(Nil) == 0.0
suma(List(-1, 2, 3)) == 4.0
suma(List(5.6)) == 5.6

def ends[A](xs: List[A]): (A, A) = {
  def last(xs: List[A]): A =
    if (xs.tail == Nil) xs.head
    else last(xs.tail)

  if (xs == Nil) throw new NoSuchElementException("Empty list")
  else if (xs.tail == Nil) (xs.head, xs.head)
  else (xs.head, last(xs.tail))
}

ends(List(1, 2, 3, 5)) == (1, 5)
ends(List(1)) == (1, 1)
try {
  ends(Nil)
}
catch {
  case e: NoSuchElementException => println(e.getMessage)
}

val posortowana: List[Int] => Boolean = xs =>
  xs == Nil || xs.tail == Nil || xs.head <= xs.tail.head && posortowana(xs.tail)

posortowana(Nil)
posortowana(List())
posortowana(List(1))
posortowana(List(1, 3, 3, 5, 6, 7))

val glue: (List[String], String) => String = (xs, sep) =>
  if (xs == Nil) ""
  else if (xs.tail != Nil) xs.head + sep + glue(xs.tail, sep)
  else xs.head

glue(Nil, "-") == ""
glue(List(), "-") == ""
glue(List("To"), "-") == "To"
glue(List("To", "jest", "napis"), "-") == "To-jest-napis"
