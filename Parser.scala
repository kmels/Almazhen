import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object Parser extends StandardTokenParsers {
  lexical.reserved ++= Set("CREATE","DATABASE")
  //lexical.delimiters ++= Set("=","/",",")

  def command:Parser[Command] = createDBCommandParser

  def createDBCommandParser:Parser[CreateDatabase] = "CREATE" ~> "DATABASE" ~> ident ^^ {
    case name => CreateDatabase(name)
  }

  def parse(s:String):Option[Command] = {
    val tokens = new lexical.Scanner(s)

    command(tokens) match {
      case Success(cmd,_) => Some(cmd)
      case _ => None
    }
  }
}