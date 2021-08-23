package part2

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}
object MonoidImplicit {
  implicit val monoidAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true

    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }
  implicit val monoidOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false

    override def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  implicit val monoidMod: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true

    override def combine(x: Boolean, y: Boolean): Boolean = (!x || y) && (!y || x)
  }

  implicit val monoidXor: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false

    override def combine(x: Boolean, y: Boolean): Boolean = (!x && y) || (x && !y)
  }
}
object Monoid {
  def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid
}
object Semigroup {
  def apply[A](implicit semigroup: Semigroup[A]): Semigroup[A] = semigroup
}
