package parser

import org.scalacheck.Prop._

class ThreeSuite extends munit.ScalaCheckSuite {

  property("digit can parse all digits") {
    forAll(Generators.digit) { digit : Char =>
      assertEquals(parseOnly (Parser.digit) (digit.toString), Some(digit))
    }
  }

  property("digit fails to parse all non-digits") {
    forAll(Generators.nonDigit) { digit : Char =>
      assertEquals(parseOnly (Parser.digit) (digit.toString), None)
    }
  }

  property("letter can parse all letters") {
    forAll(Generators.letter) { letter : Char =>
      assertEquals(parseOnly (Parser.letter) (letter.toString), Some(letter))
    }
  }

  property("letter fails to parse all non-letters") {
    forAll(Generators.nonLetter) { letter : Char =>
      assertEquals(parseOnly (Parser.letter) (letter.toString), None)
    }
  }

  property("space can parse all spaces") {
    forAll(Generators.space) { space : Char =>
      assertEquals(parseOnly (Parser.space) (space.toString), Some(space))
    }
  }

  property("space fails to parse all non-spaces") {
    forAll(Generators.nonSpace) { space : Char =>
      assertEquals(parseOnly (Parser.space) (space.toString), None)
    }
  }

}