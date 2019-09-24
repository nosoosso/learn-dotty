object MyMonoid {
  def main(args: Array[String]): Unit = {
    println(1 combine 2)
    println(sum(List(1,2,3,4,5)))
    println(unsafeSum(List(1,2,3,4,5)))
    println(unsafeSum(List[Int]())) // fail
  }
}

trait SemiGroup[T] {
  def (x: T) combine (y: T): T
}

trait Monoid[T] extends SemiGroup[T] {
  def unit: T
}

object SemiGroup {
  def apply[T](given SemiGroup[T]) = summon[SemiGroup[T]]
}

object Monoid {
  def apply[T](given Monoid[T]) = summon[Monoid[T]]
}

given Monoid[Int] {
  def (x: Int) combine (y: Int): Int = x + y
  def unit: Int = 0
}

def unsafeSum[T: SemiGroup](xs: List[T]): T =
  xs.reduceLeft(_.combine(_))

def sum[T: Monoid](xs: List[T]): T =
  xs.foldLeft(Monoid[T].unit)(_.combine(_))