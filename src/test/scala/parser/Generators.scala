package parser

import org.scalacheck.Gen
import spire.math.UByte

object Generators {

  val spaceChars =
    List(
      '\u0020','\u00A0','\u1680','\u2000','\u2001','\u2002','\u2003','\u2004','\u2005','\u2006','\u2007',
      '\u2008','\u2009','\u200A','\u202F','\u205F','\u3000'
    )

  val digit = Gen.numChar

  val nonDigit = Gen.choose(0, 0xFFFF).map(_.toChar).filter(x => !x.isDigit)

  val space = Gen.oneOf(spaceChars)

  val nonSpace = Gen.choose(0, 0xFFFF).map(_.toChar).filter(x => !spaceChars.contains(x))

  val letter = Gen.alphaChar

  val nonLetter = Gen.choose(0, 0xFFFF).map(_.toChar).filter(x => !x.isLetter)

  val int = Gen.choose(Int.MinValue, Int.MaxValue)

  val ubyte = Gen.choose[Int](UByte.MinValue.toInt, UByte.MaxValue.toInt).map(UByte(_))

  val ipv4 = 
    for {
      one <- ubyte
      two <- ubyte
      three <- ubyte
      four <- ubyte
    } yield IPv4(one, two, three, four)
}
