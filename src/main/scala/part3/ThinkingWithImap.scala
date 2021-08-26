package part3
trait Codec[A] { self =>
  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] =
    new Codec[B] {
      override def encode(value: B): String = self.encode(enc(value))

      override def decode(value: String): B = dec(self.decode(value))

    }
}

object ImplCodec {
  implicit val stringCodec: Codec[String] = new Codec[String] {
    override def encode(value: String): String = value

    override def decode(value: String): String = value
  }
  implicit def intCodec(implicit c: Codec[String]): Codec[Int] = c.imap(_.toInt, _.toString)
  implicit val doubleCodec: Codec[Double] = stringCodec.imap(_.toDouble, _.toString)

  implicit def boxCodec[A]: Codec[Box[A]] =
    stringCodec.imap[Box[A]](a => Box(a.asInstanceOf[A]), b => s"value from box = ${b.value}")

//  implicit def boxCodec2[A](implicit c: Codec[A]): Codec[Box[A]] =
//    c.imap(Box(_), _.value)
}

object Codec {
  def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)
  def decode[A](value: String)(implicit c: Codec[A]): A = c.decode(value)
}
object ThinkingWithImap extends App {
  import ImplCodec._

  println(Codec.encode(123.4))
  // res11: String = "123.4"
  println(Codec.decode[Double]("123.4"))
  // res12: Double = 123.4

  println(Codec.encode(Box(123.4)))
  // res13: String = "123.4"
  println(Codec.decode[Box[Double]]("123.4"))
  // res14: Box[Double] = Box(123.4)
}
