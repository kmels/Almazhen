import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object Parser extends StandardTokenParsers {
  lexical.reserved ++= Set("CREATE","DATABASE","ALTER","DROP","SHOW","DATABASES","USE","TABLE","PRIMARY","KEY","FOREIGN"
    ,"CHECK","INT","FLOAT","DATE","CHAR","AND","OR","NOT","RENAME","TO","ADD","COLUMN","CONSTRAINT","TABLES","COLUMNS",
    "FROM","INSERT","INTO","VALUES","UPDATE","SET","WHERE","DELETE","SELECT","ORDER","BY","ASC","DESC","NULL")
  //lexical.delimiters ++= Set("=","/",",")

  def command:Parser[Command] = createDBCommandParser

  def createDBCommandParser:Parser[CreateDatabase] = "CREATE" ~> "DATABASE" ~> ident ^^ {
    case name => CreateDatabase(name)
  }

  def alterDBCommandParser:Parser[AlterDatabase] = "ALTER" ~> "DATABASE" ~> ident ~ "RENAME" ~ "TO" ~ ident ^^ {
    case name ~ "RENAME" ~ "TO" ~ newName => AlterDatabase(name,newName)
  }

  def dropDBCommandParser:Parser[DropDatabase] = "DROP" ~> "DATABASE" ~> ident ^^ {
    case name => DropDatabase(name)
  }

  def showDBCommandParser:Parser[ShowDatabases] = "SHOW" ~> "DATABASES" ^^ {
    case _ => ShowDatabases()
  }

  def useDBCommandParser:Parser[UseDatabase] = "USE" ~> "DATABASE" ~> ident ^^ {
    case name => UseDatabase(name)
  }

//  def createTBCommandParser:Parser[DropDatabase] = "CREATE" ~> "TABLE" ~> ident ~ "(" ~ repsep(ColumnSpec,",") ~ "CONSTRAINT" ~ repsep(Restriction, ",") ~ ")" ^^ {
//    case name ~ "(" ~ List[ColumnSpec] ~ "CONSTRAINT" ~ => CreateTable(name)
//  }

  def parse(s:String):Option[Command] = {
    val tokens = new lexical.Scanner(s)

    command(tokens) match {
      case Success(cmd,_) => Some(cmd)
      case _ => None
    }
  }
}