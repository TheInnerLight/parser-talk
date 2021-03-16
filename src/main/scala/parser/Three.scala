object Three {
  case class Parser[A](f : String => Option[(A, String)])

  def parse[A](parser : Parser[A])(str : String) : Option[(A, String)] =
    parser.f(str)

  def parseOnly[A](parser : Parser[A])(str : String) : Option[A] =
    parse(parser)(str).map{ case (a,_) => a }


  object Parser {
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
  }
}