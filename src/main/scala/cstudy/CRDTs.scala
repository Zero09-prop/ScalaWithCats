package cstudy

import cats._
import cats.implicits._
import cats.kernel.CommutativeMonoid

object CRDTs {
  trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
    def combine(x: A, y: A): A
    def empty: A
  }
  object BoundedSemiLattice {
    implicit val intSemiLat: BoundedSemiLattice[Int] = new BoundedSemiLattice[Int] {
      override def combine(x: Int, y: Int): Int = x max y
      override def empty: Int = 0
    }
    implicit def setSemiLat[A](): BoundedSemiLattice[Set[A]] =
      new BoundedSemiLattice[Set[A]] {
        override def combine(x: Set[A], y: Set[A]): Set[A] = x union y

        override def empty: Set[A] = Set.empty
      }
  }
  // final case class GCounter[A](counters: Map[String, A]) {
//    def incrementMy(machine: String, amount: Int): Map[String, Int] = {
//      counters.map(x => if (x._1 == machine) (x._1, x._2 + amount) else x)
//    }
//    def mergeMy(that: GCounter): GCounter = {
//      val mapCounters = counters.map { p =>
//        val thatVal = that.counters(p._1)
//        (p._1, p._2.max(thatVal))
//      }
//      GCounter(mapCounters)
//    }
//    def total(implicit mon: CommutativeMonoid[A]): A = {
//      mon.combineAll(counters.values)
//    }
//
//    def increment(machine: String, amount: A)(implicit mon: CommutativeMonoid[A]): GCounter[A] = {
//      val value = amount |+| counters.getOrElse(machine, mon.empty)
//      GCounter(counters + (machine -> value))
//    }
//
//    def merge(that: GCounter[A])(implicit bound: BoundedSemiLattice[A]): GCounter[A] =
//      GCounter(counters |+| that.counters)
//
//  }
  trait GCounter[F[_, _], K, V] {
    def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V]
    def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]
    def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
  }
  object GCounter {
    import KeyValueStore._
    implicit def mapCounter[K, V]: GCounter[Map, K, V] =
      new GCounter[Map, K, V] {
        override def increment(f: Map[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): Map[K, V] = {
          val value = v |+| f.getOrElse(k, m.empty)
          f + (k -> value)
        }

        override def merge(f1: Map[K, V], f2: Map[K, V])(implicit b: BoundedSemiLattice[V]): Map[K, V] =
          f1 |+| f2

        override def total(f: Map[K, V])(implicit m: CommutativeMonoid[V]): V = m.combineAll(f.values)
      }
    implicit def gcounterInstance[F[_, _], K, V](implicit
        kvs: KeyValueStore[F],
        km: CommutativeMonoid[F[K, V]]
    ): GCounter[F, K, V] =
      new GCounter[F, K, V] {
        override def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V] = {
          val value = f.getOrElse(k, m.empty) |+| v
          f.put(k, value)
        }

        override def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V] = f1 |+| f2

        override def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V = f.values.combineAll
      }
    def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]): GCounter[F, K, V] = counter
  }
  trait KeyValueStore[F[_, _]] {
    def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]
    def get[K, V](f: F[K, V])(k: K): Option[V]
    def getOrElse[K, V](f: F[K, V])(k: K, default: V): V = get(f)(k).getOrElse(default)
    def values[K, V](f: F[K, V]): List[V]
  }
  object KeyValueStore {
    implicit def mapKeyValueStore: KeyValueStore[Map] =
      new KeyValueStore[Map] {
        override def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f + (k -> v)

        override def get[K, V](f: Map[K, V])(k: K): Option[V] = f.get(k)
        override def getOrElse[K, V](f: Map[K, V])(k: K, default: V): V =
          f.getOrElse(k, default)
        override def values[K, V](f: Map[K, V]): List[V] = f.values.toList
      }
    implicit class KvsOps[F[_, _], K, V](f: F[K, V]) {
      def put(key: K, value: V)(implicit kvs: KeyValueStore[F]): F[K, V] =
        kvs.put(f)(key, value)

      def get(key: K)(implicit kvs: KeyValueStore[F]): Option[V] =
        kvs.get(f)(key)

      def getOrElse(key: K, default: V)(implicit kvs: KeyValueStore[F]): V =
        kvs.getOrElse(f)(key, default)

      def values(implicit kvs: KeyValueStore[F]): List[V] =
        kvs.values(f)
    }

  }
  def main(args: Array[String]): Unit = {
    val g1 = Map("a" -> 7, "b" -> 3)
    val g2 = Map("a" -> 2, "b" -> 5)

    val counter = GCounter[Map, String, Int]

    val merged = counter.merge(g1, g2)
    println(merged)
    // merged: Map[String, Int] = Map("a" -> 7, "b" -> 5)
    val total = counter.total(merged)
    println(total)
    // total: Int = 12
  }
}
