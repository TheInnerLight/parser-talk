import cats.implicits._
import spire.math._
import spire.implicits._
import cats._
import scala.annotation.tailrec

package object parser {

  case class Parser[A](f : String => Option[(A, String)])

  def parse[A](parser : Parser[A])(str : String) : Option[(A, String)] =
    parser.f(str)

  def parseOnly[A](parser : Parser[A])(str : String) : Option[A] =
    parse(parser)(str).map{ case (a,_) => a }


  object Parser {
    implicit val parserFunctor = new Functor[Parser] {
      def map[A, B](p: Parser[A])(f: A => B): Parser[B] = 
        Parser(str => {
          val opt = parse(p)(str)
          opt match {
            case Some((a, tail)) => Some(((f(a)), tail))
            case None            => None
          }
        })
    }

    implicit val parserApplicative = new Applicative[Parser] {
      def pure[A](x: A): Parser[A] = 
        Parser(str => Some((x, str)))

      def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] =
        Parser(str => 
          for {
            (f, tail) <- parse(ff)(str)
            (a, tail2) <- parse(fa)(tail)
          } yield (f(a), tail2)
        )
    }

    implicit def parserMonad(implicit app : Applicative[Parser]) = new Monad[Parser] {
      def pure[A](x: A): Parser[A] = app.pure(x)

      def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = 
        Parser(str =>
          for {
            (a, tail) <- parse(fa)(str)
            (a2, tail2) <- parse(f(a))(tail)
          } yield (a2, tail2)
        )

      def tailRecM[A, B](a: A)(f: A => Parser[Either[A,B]]): Parser[B] = {
        @tailrec 
        def go(str : String, a : A, f : A => Parser[Either[A,B]]) : Option[(B, String)]  = {
          parse(f(a))(str) match {
            case Some((Left(a2), tail)) => go(tail, a2, f)
            case Some((Right(b), tail)) => Some((b, tail))
            case None                   => None
          }
        }
        Parser(str => go(str, a, f))
      }
    }

    def anyChar : Parser[Char] = satisfy(_ => true)

    def char(c : Char) : Parser[Char] = satisfy(c == _)

    def satisfy(cp : Char => Boolean) : Parser[Char] = Parser[Char](str =>
      str.headOption match {
        case Some(c) if cp(c)    => Some(c, str.tail)
        case Some(_)             => None
        case None                => None
      }
    )

    def digit : Parser[Char] = satisfy(_.isDigit)

    def letter : Parser[Char] = satisfy(_.isLetter)

    def space : Parser[Char] = satisfy(_.isSpaceChar)

    def digitI[A: Integral] : Parser[A] = digit.map(c => Integral[A].fromInt(c.asDigit))

    def many[A](parser : Parser[A]) : Parser[List[A]] = {
      def go(str : String, as : List[A], pA : Parser[A]) : Option[(List[A], String)] = {
        parse(pA)(str) match {
          case Some((a, tail)) => go(tail, a :: as, pA)
          case None            => Some((as.reverse, str))
        }
      }
      Parser(str => go(str, List.empty[A], parser))
    }

    def many1[A](parser : Parser[A]) =
      for {
        a <- parser
        as <- many(parser)
      } yield (a :: as)

    def integral[A : Integral] : Parser[A] = many1(digit).map(_.foldLeft(Integral[A].fromInt(0)){
      (acc, c) => acc*10 + Integral[A].fromInt(c.asDigit)
    })

  }

}