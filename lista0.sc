// Mateusz Najda

def last[A](xs: List[A]): A = {
  if (xs == Nil) throw new Exception("Empty list")
  else if (xs.tail == Nil) xs.head
  else last(xs.tail)
}

last(List(1)) == 1
last(List(1,2,3,5)) == 5
try {
  last(List())
}
catch {
  case e: Exception => println(e.getMessage)
}
try {
  last(Nil)
}
catch {
  case e: Exception => println(e.getMessage)
}
