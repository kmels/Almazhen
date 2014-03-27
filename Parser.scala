import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object Parser extends StandardTokenParsers {
  lexical.reserved ++= Set("CREATE", "DATABASE", "ALTER", "DROP", "SHOW", "DATABASES", "USE", "TABLE", "PRIMARY", "KEY", "FOREIGN"
    , "CHECK", "INT", "FLOAT", "DATE", "CHAR", "AND", "OR", "NOT", "RENAME", "TO", "ADD", "COLUMN", "CONSTRAINT", "TABLES", "COLUMNS",
    "FROM", "INSERT", "INTO", "VALUES", "UPDATE", "SET", "WHERE", "DELETE", "SELECT", "ORDER", "BY", "ASC", "DESC", "NULL")

  //lexical.delimiters ++= Set("=","/",",")

  def command: Parser[Command] = createDBCommandParser

  def createDBCommandParser: Parser[CreateDatabase] = "CREATE" ~> "DATABASE" ~> ident ^^ {
    case name => CreateDatabase(name)
  }

  def alterDBCommandParser: Parser[AlterDatabase] = "ALTER" ~> "DATABASE" ~> ident ~ "RENAME" ~ "TO" ~ ident ^^ {
    case name ~ "RENAME" ~ "TO" ~ newName => AlterDatabase(name, newName)
  }

  def dropDBCommandParser: Parser[DropDatabase] = "DROP" ~> "DATABASE" ~> ident ^^ {
    case name => DropDatabase(name)
  }

  def showDBCommandParser: Parser[ShowDatabases] = "SHOW" ~> "DATABASES" ^^ {
    case _ => ShowDatabases()
  }

  def useDBCommandParser: Parser[UseDatabase] = "USE" ~> "DATABASE" ~> ident ^^ {
    case name => UseDatabase(name)
  }


  def createTBCommandParser: Parser[CreateTable] = "CREATE" ~> "TABLE" ~> ident ~ "(" ~ repsep(columnSpec, ",") ~ "CONSTRAINT" ~ repsep(restriction, ",") <~ ")" ^^ {
    case name ~ "(" ~ cs ~ "CONSTRAINT" ~ re => CreateTable(name, cs, re)
  }

  def renameTBCommandParser: Parser[RenameTable] = "ALTER" ~> "TABLE" ~> ident ~ "RENAME" ~ "TO" ~ ident ^^ {
    case name ~ "RENAME" ~ "TO" ~ newName => RenameTable(name, newName)
  }

  def addColumnCommandParser: Parser[AddColumn] = "ALTER" ~> "TABLE" ~> ident ~ "ADD" ~ "COLUMN" ~ columnSpec ~ "CONSTRAINT" ~ repsep(restriction, ",") ^^{
    case name ~ "ADD" ~ "COLUMN" ~ colName ~ "CONSTRAINT" ~ theRestriction => AddColumn(name, colName, theRestriction)
  }

  def addConstraintCommandParser: Parser[AddConstraint] = "ALTER" ~> "TABLE" ~> ident ~ "ADD" ~ "CONSTRAINT" ~ restriction ^^ {
    case name ~ "ADD" ~ "CONSTRAINT" ~ theRestriction => AddConstraint(name, theRestriction)
  }

  def dropColumnCommandParser: Parser[DropColumn] = "ALTER" ~> "TABLE" ~> ident ~ "DROP" ~ "COLUMN" ~ ident ^^ {
    case name ~ "DROP" ~ "COLUMN" ~ colName => DropColumn(name, colName)
  }

  def dropConstraintCommandParser: Parser[DropConstraint] = "ALTER" ~> "TABLE" ~> ident ~ "DROP" ~ "CONSTRAINT" ~ ident ^^ {
    case name ~ "DROP" ~ "CONSTRAINT" ~ consName => DropConstraint(name, consName)
  }

  def dropTableCommandParser: Parser[DropTable] = "DROP" ~> "TABLE" ~> ident ^^ {
    case tableName => DropTable(tableName)
  }

  def showTablesCommandParser: Parser[ShowTables] = "SHOW" ~> "TABLES" ^^{
    case _ => ShowTables()
  }

  def showColumnsCommandParser: Parser[ShowColumns] = "SHOW" ~> "COLUMNS" ~> "FROM" ~> ident ^^{
    case tableName => ShowColumns(tableName)
  }

  def insertCommandParser: Parser[Insert] = "INSERT" ~> "INTO" ~>  ident ~ "VALUES" ~ "(" ~ repsep(ident/*string*/, ",") <~ ")" ^^{
    case tableName ~ "VALUES" ~ "(" ~ newValues => Insert(tableName, newValues)
  }

  def insertWithColumnsCommandParser: Parser[InsertWithColumns] = "INSERT" ~> "INTO" ~>  ident ~"("~repsep(ident/*string*/,",")~")"~ "VALUES" ~ "(" ~ repsep(ident, ",") <~ ")" ^^{
    case tableName ~"("~theColumns~")"~ "VALUES" ~ "(" ~ newValues => InsertWithColumns(tableName, theColumns, newValues)
  }

  def updateCommandParser: Parser[Update] = "UPDATE" ~> ident ~ "SET" ~ repsep(colValuePair,",")~"WHERE" ~ ident/*predicate*/ ^^{ /// definir predicate
    case tableName ~"("~theColumns~")"~ "VALUES" ~ "(" ~ newValues => Update(tableName, theColumns, newValues)
  }

  def deleteCommandParser: Parser[Delete] = "DELETE"~"FROM" ~> ident ~ "WHERE" ~ ident/*predicate*/ ^^{ /// definir predicate
    case tableName ~"WHERE"~thePredicate => Delete(tableName, thePredicate)
  }

  def selectCommandParser: Parser[Select] = "SELECT" ~> repsep(ident,",")~"FROM" ~ ident  ~ "WHERE" ~ ident/*predicate*/ ~ "ORDER" ~ "BY" ~ repsep(orderByExpr,",")  ^^{ /// definir predicate
    case columnList~"FROM" ~ tableName  ~ "WHERE" ~ thePredicate/*predicate*/ ~ "ORDER" ~ "BY" ~ theOrderExpr=> Select(columnList, tableName, thePredicate)
  }

  def selectCommandParser: Parser[Select] = "SELECT" ~> "*"~>"FROM" ~ ident  ~ "WHERE" ~ ident/*predicate*/ ~ "ORDER" ~ "BY" ~ repsep(orderByExpr,",")  ^^{ /// definir predicate
    case tableName  ~ "WHERE" ~ thePredicate/*predicate*/ ~ "ORDER" ~ "BY" ~ theOrderExpr=> SelectAll(tableName, thePredicate, theOrderExpr)
  }

  def orderByExpr: Parser[ColValuePair] = ident/*expr*/~("ASC"|"DESC")^^ {
    case expression ~ "ASC" => OrderExpr(expression, "asc")
    case expression ~ "DESC" => OrderExpr(expression, "desc")
  }

  def colValuePair: ColValuePair = ident~"="~ident ^^ {
    case colName ~ "=" ~ newValue => ColValuePair(colName , newValue)
  }

  def columnSpec: Parser[ColumnSpec] = ident ~ ("INT" | "FLOAT" | "DATE" | "CHAR") ^^ {
    case name ~ "INT" => ColumnInt //fix constructor and type definition
    case name ~ "FLOAT" => ColumnFloat //fix constructor and type definition
    case name ~ "DATE" => ColumnDate //fix constructor and type definition
    case name ~ "CHAR" => ColumnChar
  } //fix constructor and type definition

  def restriction:Parser[Restriction] = (pk_restriction | fk_restriction | ch_restriction) ^^ {
      case _ => Restriction() //fix constructor and type definition
  }

  def pk_restriction:Parser[Pk_key] = "PK_"ident~"PRIMARY"~"KEY"~"(" repsep(ident,",")<~")" ^^ {
    case name ~"PRIMARY"~"KEY"~"(" ~ columns => Pk_key(name, columns)
  }

  def fk_restriction:Parser[Fk_key] = "FK_"ident~"FOREIGN"~"KEY"~"(" repsep(ident,",")~")"~"REFERENCES"~ident~"("~repsep(ident,",")<~")" {
    case name ~"FOREIGN"~"KEY"~"(" ~ columns ~")"~"REFERENCES"~referencedTableName~"("~ referencedColumns => Fk_key(name, columns)
  }

  def ch_restriction:Parser[Ch_key] = "CH_"ident~"CHECK"~"(" predicate ~ ")" ^^ {
    case name ~"CHECK"~"(" ~ thePredicate ~ ")" =>Ch_key(name, thePredicate)
  }

  def parse(s:String):Option[Command] = {
    val tokens = new lexical.Scanner(s)

    command(tokens) match {
      case Success(cmd,_) => Some(cmd)
      case _ => None
    }
  }
}