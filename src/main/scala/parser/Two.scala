object Two {
  case class Parser[A](f : String => Option[(A, String)])

  def anyChar : Parser[Char] = Parser[Char](str =>
      str.headOption.map(c => (c, str.tail))
    )

  def parse[A](parser : Parser[A])(str : String) : Option[(A, String)] =
    parser.f(str)

  def parseOnly[A](parser : Parser[A])(str : String) : Option[A] =
    parse(parser)(str).map{ case (a,_) => a }


  
  object Parser {
    def char(c : Char) = Parser[Char](str =>
      str.headOption match {
        case Some(cc) if c == cc => Some(cc, str.tail)
        case Some(_)             => None
        case None                => None
      }
    )

  }
}