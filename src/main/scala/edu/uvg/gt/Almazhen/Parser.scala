package edu.uvg.gt.Almazhen

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object Parser extends StandardTokenParsers {
  lexical.reserved ++= Set("CREATE", "DATABASE", "ALTER", "DROP", "SHOW", "DATABASES", "USE", "TABLE", "PRIMARY", "KEY", "FOREIGN"
    , "CHECK", "INT", "FLOAT", "DATE", "CHAR", "AND", "OR", "NOT", "RENAME", "TO", "ADD", "COLUMN", "CONSTRAINT", "TABLES", "COLUMNS",
    "FROM", "INSERT", "INTO", "VALUES", "UPDATE", "SET", "WHERE", "DELETE", "SELECT", "ORDER", "BY", "ASC", "DESC", "NULL")

  //lexical.delimiters ++= Set("=","/",",")

  def command: Parser[Command] = createDBCommandParser ||| showDBCommandParser ||| dropDBCommandParser |||
  	useDBCommandParser ||| alterDBCommandParser

  def createDBCommandParser: Parser[CreateDatabase] = "CREATE" ~> "DATABASE" ~> ident ^^ {
    case name => CreateDatabase(name)
  }

  def alterDBCommandParser: Parser[AlterDatabase] = "ALTER" ~> "DATABASE" ~> ident ~ "RENAME" ~ "TO" ~ ident ^^ {
    case name ~ "RENAME" ~ "TO" ~ newName => AlterDatabase(name, newName)
  }

  def dropDBCommandParser: Parser[DropDatabase] = "DROP" ~> "DATABASE" ~> ident ^^ {
    case name => DropDatabase(name)
  }

  def showDBCommandParser: Parser[ShowDatabases] = "SHOW" ~ "DATABASES" ^^ {
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

  def updateCommandParser: Parser[UpdateCommand] = "UPDATE" ~> ident ~ "SET" ~ repsep(assignment,",") ~ "WHERE" ~ predicate ^^{ /// definir predicate
    case tableName ~ "SET" ~ theColumns ~ "WHERE" ~ newValues => UpdateCommand(tableName, theColumns, newValues)
  }

  def predicate: Parser[Predicate] = ident ^^ { case _ => Predicate()}

  def deleteCommandParser: Parser[Delete] = "DELETE"~"FROM" ~> ident ~ "WHERE" ~ predicate ^^{
    case tableName ~"WHERE"~thePredicate => Delete(tableName, thePredicate)
  }

  def selectSome: Parser[SelectCommand] = "SELECT" ~> repsep(ident,",")~"FROM" ~ ident  ~ "WHERE" ~ predicate ~ "ORDER" ~ "BY" ~ repsep(orderByExpr,",")  ^^{ /// definir predicate
    case projections ~ "FROM" ~ tableName  ~ "WHERE" ~ thePredicate ~ "ORDER" ~ "BY" ~ orderby => SelectCommand(projections, tableName, thePredicate, orderby)
  }

  def selectAll: Parser[SelectCommand] = "SELECT" ~> "*" ~> "FROM" ~> ident ~ "WHERE" ~ predicate ~ "ORDER" ~ "BY" ~ repsep(orderByExpr,",")  ^^{ /// definir predicate
    case tableName ~ "WHERE" ~ thePredicate ~ "ORDER" ~ "BY" ~ orderby => SelectCommand(List(), tableName, thePredicate, orderby)
  }

  def orderByExpr: Parser[OrderBy] = ident/*expr*/~("ASC"|"DESC")^^ {
    case expression ~ "ASC" => OrderBy(expression, "asc")
    case expression ~ "DESC" => OrderBy(expression, "desc")
  }

  def assignment: Parser[Assignment] = ident~"="~ident ^^ {
    case colName ~ "=" ~ newValue => Assignment(colName , newValue)
  }

  def columnSpec: Parser[ColumnDefinition] = ident ~ ("INT" | "FLOAT" | "DATE" | "CHAR") ^^ {
    case name ~ "INT" => ColumnDefinition(name, IntType)
    case name ~ "FLOAT" => ColumnDefinition(name, FloatType)
    case name ~ "DATE" => ColumnDefinition(name, DateType)
    case name ~ "CHAR" => ColumnDefinition(name, CharType)
  }

  def restriction:Parser[Restriction] = (pk_restriction | fk_restriction | ch_restriction) ^^ {
    case _ => Restriction() //fix constructor and type definition
  }

  def pk_restriction:Parser[Pk_key] = pkNameParser ~ "PRIMARY" ~ "KEY" ~ "(" ~ repsep(ident,",") <~")" ^^ {
    case pkName ~ "PRIMARY" ~ "KEY" ~ "(" ~ columns => Pk_key(pkName, columns)
  }

  def pkNameParser: Parser[String] = ident // """Pk_(\w+)?""".r ^^ { i => i}
  def fkNameParser: Parser[String] = ident// """Fk_(\w+)?""".r ^^ { i => i}
  def chNameParser: Parser[String] = ident //"""Pk_(\w+)?""".r ^^ { i => i}

  def fk_restriction:Parser[Fk_key] = fkNameParser ~ "FOREIGN"~"KEY"~"(" ~ repsep(ident,",") ~ ")" ~ "REFERENCES" ~ ident ~ "("~repsep(ident,",")<~")" ^^ {
    case fkName ~"FOREIGN"~"KEY"~"(" ~ columns ~")"~"REFERENCES"~referencedTableName~"("~ referencedColumns => Fk_key(fkName, columns.zip(referencedColumns))
  }

  def ch_restriction:Parser[Ch_key] = chNameParser ~ "CHECK" ~ "(" ~ predicate ~ ")" ^^ {
    case name ~"CHECK"~"(" ~ thePredicate ~ ")" => Ch_key(name, thePredicate)
  }

  def parse(s:String):Option[Command] = {
    val tokens = new lexical.Scanner(s)

    command(tokens) match {
      case Success(cmd,_) => Some(cmd)
      case _ => None
    }
  }
}
