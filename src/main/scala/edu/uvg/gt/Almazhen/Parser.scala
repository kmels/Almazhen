package edu.uvg.gt.Almazhen

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharSequenceReader.EofCh
import scala.collection.mutable.HashSet

object Parser extends StandardTokenParsers {
  override val lexical = new Lexer

  def command: Parser[Command] =
  	/* kmels */
  	createDB ||| showDatabases ||| dropDB ||| useDB ||| alterDB ||| addColumn ||| insertINTO |||
  	/* paulo */
  	createTable ||| showTables ||| dropTable ||| renameTable ||| showColumns ||| dropColumn ||| addConstraint ||| dropConstraint

/*  def tst: Parser[Command] = pk_restriction ^^ {
    case a => {
      println("Parsed kmels")
      println(a)
      ShowTables()
    }
  }*/

  def createDB: Parser[CreateDatabase] = "CREATE" ~> "DATABASE" ~> ident ^^ {
    case name => CreateDatabase(name)
  }

  def alterDB: Parser[AlterDatabase] = "ALTER" ~> "DATABASE" ~> ident ~ "RENAME" ~ "TO" ~ ident ^^ {
    case name ~ "RENAME" ~ "TO" ~ newName => AlterDatabase(name, newName)
  }

  def dropDB: Parser[DropDatabase] = "DROP" ~> "DATABASE" ~> ident ^^ {
    case name => DropDatabase(name)
  }

  def showDatabases: Parser[ShowDatabases] = "SHOW" ~ "DATABASES" ^^ {
    case _ => ShowDatabases()
  }

  def useDB: Parser[UseDatabase] = "USE" ~> "DATABASE" ~> ident ^^ {
    case name => UseDatabase(name)
  }

  def createTable: Parser[CreateTable] = "CREATE" ~> "TABLE" ~> ident ~ "(" ~ repsep(columnSpec, ",") ~ "CONSTRAINT" ~ repsep(restriction, ",") <~ ")" ^^ {
    case name ~ "(" ~ cs ~ "CONSTRAINT" ~ re => CreateTable(name, cs, re)
  }

  def renameTable: Parser[RenameTable] = "ALTER" ~> "TABLE" ~> ident ~ "RENAME" ~ "TO" ~ ident ^^ {
    case name ~ "RENAME" ~ "TO" ~ newName => RenameTable(name, newName)
  }

  def addColumn: Parser[AddColumn] = "ALTER" ~> "TABLE" ~> ident ~ "ADD" ~ "COLUMN" ~ columnSpec ~ "CONSTRAINT" ~ repsep(restriction, ",") ^^{
    case table_name ~ "ADD" ~ "COLUMN" ~ colName ~ "CONSTRAINT" ~ theRestriction => AddColumn(table_name, colName, theRestriction)
  }

  def addConstraint: Parser[AddConstraint] = "ALTER" ~> "TABLE" ~> ident ~ "ADD" ~ "CONSTRAINT" ~ restriction ^^ {
    case name ~ "ADD" ~ "CONSTRAINT" ~ theRestriction => AddConstraint(name, theRestriction)
  }

  def dropColumn: Parser[DropColumn] = "ALTER" ~> "TABLE" ~> ident ~ "DROP" ~ "COLUMN" ~ ident ^^ {
    case name ~ "DROP" ~ "COLUMN" ~ colName => DropColumn(name, colName)
  }

  def dropConstraint: Parser[DropConstraint] = "ALTER" ~> "TABLE" ~> ident ~ "DROP" ~ "CONSTRAINT" ~ constraintName ^^ {
    case name ~ "DROP" ~ "CONSTRAINT" ~ consName => DropConstraint(name, consName)
  }

  def dropTable: Parser[DropTable] = "DROP" ~> "TABLE" ~> ident ^^ {
    case tableName => DropTable(tableName)
  }

  def showTables: Parser[ShowTables] = "SHOW" ~> "TABLES" ^^{
    case _ => ShowTables()
  }

  def showColumns: Parser[ShowColumns] = "SHOW" ~> "COLUMNS" ~> "FROM" ~> ident ^^{
    case tableName => ShowColumns(tableName)
  }

  def insertINTO: Parser[Insert] = "INSERT" ~> "INTO" ~> ident ~ maybeColumnList ~ "VALUES" ~ "(" ~ valueList <~ ")" ^^{
    case tableName ~ columnList ~ "VALUES" ~ "(" ~ values => Insert(tableName, columnList, values)
  }
  
  def maybeColumnList: Parser[Option[List[String]]] = opt(repsep(ident,","))
  def valueList: Parser[List[String]] = repsep(aValueExpr, ",")
  def aValueExpr: Parser[String] = numericLit ||| date 
  
  def insertWithColumnsCommandParser: Parser[InsertWithColumns] = "INSERT" ~> "INTO" ~>  ident ~"("~repsep(ident/*string*/,",")~")"~ "VALUES" ~ "(" ~ repsep(ident, ",") <~ ")" ^^{
    case tableName ~"("~theColumns~")"~ "VALUES" ~ "(" ~ newValues => InsertWithColumns(tableName, theColumns, newValues)
  }

  def updateCommandParser: Parser[UpdateCommand] = "UPDATE" ~> ident ~ "SET" ~ repsep(assignment,",") ~ "WHERE" ~ predicate ^^{ /// definir predicate
    case tableName ~ "SET" ~ theColumns ~ "WHERE" ~ newValues => UpdateCommand(tableName, theColumns, newValues)
  }

  def predicate: Parser[Predicate] = ident ^^ { case _ => Predicate("")}

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

  def columnSpec: Parser[ColumnDefinition] = ident ~ aType ^^ {
    case id ~ typ => ColumnDefinition(id, typ)
  }

  def aType: Parser[AZtype] = intType ||| floatType ||| dateType ||| varchar

  def intType = "INT" ^^ { case "INT" => IntType}
  def floatType = "FLOAT" ^^ { case "FLOAT" => FloatType}
  def dateType = "DATE" ^^ { case "DATE" => DateType}
  def varchar: Parser[VARCHAR] = "CHAR" ~> "(" ~> numericLit <~ ")" ^^ {
    case size => VARCHAR(size.toInt)
  }

  def restriction: Parser[Constraint] = pk_restriction ||| fk_restriction ||| ch_restriction
  def constraintName: Parser[String] = primaryKeyName ||| foreignKeyName ||| checkName

  def pk_restriction:Parser[Pk_key] = primaryKeyName ~ "PRIMARY" ~ "KEY" ~ "(" ~ repsep(ident,",") <~")" ^^ {
    case pkName ~ "PRIMARY" ~ "KEY" ~ "(" ~ columns => Pk_key(pkName, columns)
  }

  def fk_restriction:Parser[Fk_key] = foreignKeyName ~ "FOREIGN" ~ "KEY" ~ "(" ~ repsep(ident,",") ~ ")" ~ "REFERENCES" ~ ident ~ "("~repsep(ident,",")<~")" ^^ {
    case fkName ~"FOREIGN"~"KEY"~"(" ~ columns ~")"~"REFERENCES"~referencedTableName~"("~ referencedColumns => Fk_key(fkName, referencedTableName, columns.zip(referencedColumns))
  }

  def ch_restriction:Parser[Ch_key] = checkName ~ "CHECK" ~ "(" ~ predicate ~ ")" ^^ {
    case name ~"CHECK"~"(" ~ thePredicate ~ ")" => Ch_key(name, thePredicate)
  }

  def primaryKeyName: Parser[String] = elem("primary key", _.isInstanceOf[lexical.PkNameLit]) ^^ (_.chars)
  def foreignKeyName: Parser[String] = elem("foreign key", _.isInstanceOf[lexical.FkNameLit]) ^^ (_.chars)
  def checkName: Parser[String] = elem("check key", _.isInstanceOf[lexical.CheckNameLit]) ^^ (_.chars)

  def date: Parser[String] = elem("date", _.isInstanceOf[lexical.DateLit]) ^^ (_.chars)
  
  def parse(s:String):Option[Command] = {
    val tokens = new lexical.Scanner(s)

    command(tokens) match {
      case Success(cmd,_) => Some(cmd)
      case _ => None
    }
  }
}

class Lexer extends StdLexical{
  override def token:Parser[Token] =
    (
    'P' ~> 'K' ~> '_' ~> rep(identChar | digit)         ^^ { name => PkNameLit(name.mkString)}
    | 'F' ~> 'K' ~> '_' ~> rep(identChar | digit)         ^^ { name => FkNameLit(name.mkString)}
    | 'C' ~> 'H' ~> '_' ~> rep(identChar | digit)         ^^ { name => CheckNameLit(name.mkString)}
    | year ~ '-' ~ month ~ '-' ~ day ^^ { case y ~ '-' ~ m ~ '-' ~ d => DateLit(d, m, y, s"$y-$m-$d")}
    | identChar ~ rep( identChar | digit )              ^^ { case first ~ rest => processIdent(first :: rest mkString "") }
    | digit ~ rep( digit )                              ^^ { case first ~ rest => NumericLit(first :: rest mkString "") }
    | '\'' ~ (letter|digit) ~ '\''                              ^^ { case '\'' ~ char ~ '\'' => CharLit(char.toString) }
    | '\'' ~ rep( chrExcept('\'', '\n', EofCh) ) ~ '\'' ^^ { case '\'' ~ chars ~ '\'' => StringLit(chars mkString "") }
    | '\"' ~ rep( chrExcept('\"', '\n', EofCh) ) ~ '\"' ^^ { case '\"' ~ chars ~ '\"' => StringLit(chars mkString "") }
    | EofCh                                             ^^^ EOF
    | '\'' ~> failure("unclosed string literal")
    | '\"' ~> failure("unclosed string literal")
    | delim
    | failure("illegal character")
    )
    
  def year: Parser[Int] = digit ~ digit ~ digit ~ digit ^^ { case y1 ~ y2 ~ y3 ~ y4 => List(y1,y2,y3,y4).mkString("").toInt }
  def month: Parser[Int] = digit ~ digit ^^ { case m1 ~ m2 => List(m1,m2).mkString("").toInt }
  def day: Parser[Int] = digit ~ digit ^^ { case d1 ~ d2 => List(d1,d2).mkString("").toInt }
  
  case class CharLit(val chars:String) extends Token {
    override def toString = "'"+chars+"'"
  }

  case class PkNameLit(val chars: String) extends Token { override def toString = "'"+chars+"'"}
  case class FkNameLit(val chars: String) extends Token { override def toString = "'"+chars+"'"}
  case class CheckNameLit(val chars: String) extends Token { override def toString = "'"+chars+"'"}

  case class DateLit(day: Int, month: Int, year: Int, val chars: String) extends Token { override def toString = s"$year-$month-$day"}
  
  reserved ++= Set("CREATE", "DATABASE", "ALTER", "DROP", "SHOW", "DATABASES", "USE", "TABLE", "PRIMARY", "KEY", "FOREIGN"
    , "CHECK", "INT", "FLOAT", "DATE", "CHAR", "AND", "OR", "NOT", "RENAME", "TO", "ADD", "COLUMN", "CONSTRAINT", "TABLES", "COLUMNS",
    "FROM", "INSERT", "INTO", "VALUES", "UPDATE", "SET", "WHERE", "DELETE", "SELECT", "ORDER", "BY", "ASC", "DESC", "NULL")


  delimiters ++= Set("(",")",",")
}
