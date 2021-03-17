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
    implicit val catsInstances = new Functor[Parser] with Applicative[Parser] with Monad[Parser] with Alternative[Parser] {
      override def map[A, B](p: Parser[A])(f: A => B): Parser[B] = 
        Parser(str => {
          val opt = parse(p)(str)
          opt match {
            case Some((a, tail)) => Some(((f(a)), tail))
            case None            => None
          }
        })

      def pure[A](x: A): Parser[A] = 
        Parser(str => Some((x, str)))

      override def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] =
        Parser(str => 
          for {
            (f, tail) <- parse(ff)(str)
            (a, tail2) <- parse(fa)(tail)
          } yield (f(a), tail2)
        )

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

      def combineK[A](x: Parser[A], y: Parser[A]): Parser[A] = 
        Parser(str =>
          parse(x)(str) match {
            case None => parse(y)(str)
            case some => some
          }
        )

      def empty[A]: Parser[A] = fail
    }

    def fail[A] : Parser[A] = Parser(_ => None)

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

    def natural[A : Integral] : Parser[A] = 
      many1(digit).map(_.foldLeft(Integral[A].fromInt(0)){
        (acc, c) => acc*10 + Integral[A].fromInt(c.asDigit)
      })

    def integral[A : Integral] : Parser[A] = 
      for {
        sign <- optional(char('-'))
        sigNum = sign.map(_ => -1).getOrElse(1)
        posIntegral <- natural
      } yield sigNum * posIntegral

    def optional[A](parser : Parser[A]) : Parser[Option[A]] = 
      parser.map[Option[A]](Some(_)) <+> Applicative[Parser].pure(None)

    def choice[A](parserList : List[Parser[A]]) : Parser[A] =
      parserList.foldLeft(fail[A])((acc, p) => acc <+> p)

    val ubyte : Parser[UByte] = natural[Byte].map(UByte(_))

    val ipv4 : Parser[IPv4] =
      for {
        one <- ubyte
        _ <- char('.')
        two <- ubyte
        _ <- char('.')
        three <- ubyte
        _ <- char('.')
        four <- ubyte
      } yield IPv4(one, two, three, four)

  }

}