package parser

import spire.math.UByte
import cats.Show

case class IPv4(one : UByte, two : UByte, three : UByte, four : UByte)

object IPv4 {
  implicit val showIPv4 : Show[IPv4] = new Show[IPv4] {
    override def show(ip: IPv4): String = s"${ip.one}.${ip.two}.${ip.three}.${ip.four}"
  }
}
