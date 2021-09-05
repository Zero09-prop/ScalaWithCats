package cstudy

import cats.Applicative
import cats._
import cats.implicits._

import scala.concurrent.Future

object TestingAsynchronousCode {
  type Id[A] = A

  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }
  trait RealUptimeClient extends UptimeClient[Future] {
    def getUptime(hostname: String): Future[Int]
  }
  trait TestUptimeClient extends UptimeClient[Id] {
    def getUptime(hostname: String): Id[Int]
  }
  class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
    def getTotalUptime(hostnames: List[String]): F[Int] =
      hostnames.traverse(client.getUptime).map(_.sum)
  }
  class TestUptime(hosts: Map[String, Int]) extends TestUptimeClient {
    def getUptime(hostname: String): Id[Int] =
      hosts.getOrElse(hostname, 0)
  }
  def testTotalUptime(): Unit = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptime(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }

  def main(args: Array[String]): Unit = {
    testTotalUptime()
  }
}
