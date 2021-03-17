package parser

import org.scalacheck.Prop._
import org.scalacheck.Gen

class FiveSuite extends munit.ScalaCheckSuite {

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

  property("many can parse many digits") {
    forAll(Gen.listOf(Generators.digit)) { digits : List[Char] =>
      assertEquals(parseOnly (Parser.many (Parser.digit)) (digits.mkString), Some(digits))
    }
  }

  // this one fails because an empty list is valid!
  // property("many fails for non digits") {
  //   forAll(Gen.listOf(Generators.nonDigit)) { digits : List[Char] =>
  //     assertEquals(parseOnly (Parser.many (Parser.digit)) (digits.mkString), None)
  //   }
  // }

  property("many digits is empty for non digits") {
    forAll(Gen.listOf(Generators.nonDigit)) { digits : List[Char] =>
      assertEquals(parseOnly (Parser.many (Parser.digit)) (digits.mkString), Some(List.empty))
    }
  }

  property("many1 fails for non digits") {
    forAll(Gen.listOf(Generators.nonDigit)) { digits : List[Char] =>
      assertEquals(parseOnly (Parser.many1 (Parser.digit)) (digits.mkString), None)
    }
  }

  property("integral succeeds for all integers") { 
    forAll(Generators.int) { i =>
      assertEquals(parseOnly (Parser.integral[Int]) (i.toString), Some(i))
    }
  }

}