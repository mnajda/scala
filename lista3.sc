// Mateusz Najda

def exists[A](xs: List[A])(p: A => Boolean): Boolean = xs match {
  case h :: t => p(h) || exists(t)(p)
  case Nil => false
}

def existsFoldLeft[A](xs: List[A])(p: A => Boolean): Boolean = {
  xs.foldLeft(false)((res, elem) => res || p(elem))
}

def existsFoldRight[A](xs: List[A])(p: A => Boolean): Boolean = {
  xs.foldRight(false)((elem, res) => res || p(elem))
}

exists(List())(_ == 2)
exists(Nil)(Nil)
exists(List(1))(_ == 1)
exists(List(5, 1, 2, 3))(_ == 2)

existsFoldLeft(List())(_ == 2)
existsFoldLeft(Nil)(Nil)
existsFoldLeft(List(1))(_ == 1)
existsFoldLeft(List(5, 1, 2, 3))(_ == 2)

existsFoldRight(List())(_ == 2)
existsFoldRight(Nil)(Nil)
existsFoldRight(List(1))(_ == 1)
existsFoldRight(List(5, 1, 2, 3))(_ == 2)

def filter[A](xs: List[A])(p: A => Boolean): List[A] =
  xs.foldRight(List[A]())((elem, res) => if (p(elem)) elem :: res else res)

filter(Nil)(Nil) == Nil
filter(List(1))(_ > 0) == List(1)
filter(List(2,7,1,3,7,8,4,1,6,9))(_ > 3) == List(7,7,8,4,6,9)

def remove1[A](xs: List[A])(p: A => Boolean): List[A] = xs match {
  case h :: t => if (p(h)) t else h :: remove1(t)(p)
  case Nil => Nil
}

def remove1TailRec[A](xs: List[A])(p: A => Boolean): List[A] = {
  def rec(acc: List[A], list: List[A]): List[A] = list match {
    case h :: t => if (p(h)) t.reverse_:::(acc) else rec(h :: acc, t)
    case Nil => acc.reverse
  }

  rec(List(), xs)
}

remove1(Nil)(Nil) == Nil
remove1(List(1,3,5))(_ == 2) == List(1,3,5)
remove1(List(1,2,3,2,5))(_ == 2) == List(1,3,2,5)
remove1(List(1,3,2,3,2,5))(_ == 2) == List(1,3,3,2,5)

remove1TailRec(Nil)(Nil) == Nil
remove1TailRec(List(1,3,5))(_ == 2) == List(1,3,5)
remove1TailRec(List(1,2,3,2,5))(_ == 2) == List(1,3,2,5)
remove1TailRec(List(1,3,2,3,2,5))(_ == 2) == List(1,3,3,2,5)

def splitAt[A](xs: List[A])(n: Int): (List[A], List[A]) = {
  def rec(acc: List[A], list: List[A], count: Int): (List[A], List[A]) =
    list match {
      case h :: t => if (count == 0) (acc.reverse, list)
                     else rec(h :: acc, t, count - 1)
      case Nil => (acc.reverse, Nil)
  }

  rec(List(), xs, n)
}

splitAt(Nil)(2) == (Nil, Nil)
splitAt(List('a'))(1) == (List('a'), List())
splitAt(List('a'))(3) == (List('a'), List())
splitAt(List('a','b','c','d','e'))(2) == (List('a', 'b'), List('c', 'd', 'e'))
