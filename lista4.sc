// Mateusz Najda

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

val tree = Node(1, Node(2, Empty, Node(3, Empty, Empty)), Empty)
val tree2 = Node(1, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(3, Node(6, Empty, Empty), Node(7, Empty, Empty)))

def sumBT(bt: BT[Int]): Int = bt match {
  case Node(value, left, right) => value + sumBT(left) + sumBT(right)
  case Empty => 0
}

sumBT(tree) == 6
sumBT(tree2) == 28

def foldBT[A, B](f: A => (B, B) => B)(acc: B)(bt: BT[A]): B = bt match {
  case Node(value, left, right) => f(value)(foldBT(f)(acc)(left), foldBT(f)(acc)(right))
  case Empty => acc
}

def sumBTfold(bt: BT[Int]): Int =
  foldBT[Int, Int](value => (left, right) => value + left + right)(0)(bt)

sumBTfold(tree) == 6
sumBTfold(tree2) == 28

def inorderBTfold(bt: BT[Int]): List[Int] =
  foldBT[Int, List[Int]](value => (left, right) => left ::: value :: right)(List())(bt)

inorderBTfold(tree) == List(2, 3, 1)
inorderBTfold(tree2) == List(4, 2, 5, 1, 6, 3, 7)

def mapBT[A, B](f: A => B)(tree: BT[A]) : BT[B] =
  foldBT[A, BT[B]](value => (left, right) => Node(f(value), left, right))(Empty)(tree)

mapBT((v: Int) => 2 * v)(tree: BT[Int]) == Node(2,Node(4,Empty,Node(6,Empty,Empty)),Empty)
mapBT((v: Int) => v + 6)(tree2: BT[Int]) == Node(7, Node(8, Node(10, Empty, Empty), Node(11, Empty, Empty)), Node(9, Node(12, Empty, Empty), Node(13, Empty, Empty)))

sealed trait Graphs[A]
case class Graph[A](succ: A => List[A]) extends Graphs[A]

val g = Graph((i: Int) =>
  i match {
    case 0 => List(3)
    case 1 => List(0,2,4)
    case 2 => List(1)
    case 3 => List(5)
    case 4 => List(0,2)
    case 5 => List(3)
    case n => throw
      new NoSuchElementException("Graph g: node " + n + " doesn't exist")}
)

def pathExists[A](g: Graph[A])(from: A, to: A): Boolean = {
  def find(visited: List[A], notVisited: List[A]): Boolean = {
      notVisited match {
        case h :: t =>
          (h == to) || (if (visited.contains(h))
            find(visited, t)
          else
            find(h :: visited, t ::: (g succ h)))
        case Nil => false
      }
    }

  find(List(), List(from))
}

pathExists(g)(4,1) == true
pathExists(g)(0,4) == false
pathExists(g)(3, 0) == false
