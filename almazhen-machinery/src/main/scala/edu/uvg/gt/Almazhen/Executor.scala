package edu.uvg.gt.Almazhen

import java.util.Date
import java.io.BufferedReader
import java.io.InputStreamReader

class ExecutionResult
case class AffectedRows(n: Int) extends ExecutionResult{
  override def toString = if (n > 0) "Affected " + n + " rows" else ""
}

case class Error(msg: String) extends ExecutionResult{
  override def toString = msg
}

case class ShowRows(colnames: List[String], rows: List[Row]) extends ExecutionResult {
  override def toString = colnames.mkString("\t") + "\n" + rows.mkString("\n") +  "\n" + "SELECT("+(rows.size)+") "
}

case class Benchmarked(res: ExecutionResult, milliseconds: Long) extends ExecutionResult{
  override def toString = res + "\n... in " + milliseconds + "ms"
}

object Executor {
	var doIO = true;
	
	def log(s: String) = Rows.log(s)
	
	final def DATABASE_DOES_NOT_EXIST(dbname: String) = Error(s"Database $dbname doesn't exist")
	final def DATABASE_ALREADY_EXISTS(dbname: String) = Error(s"Database $dbname already exists")
	final def TABLE_ALREADY_EXISTS(dbname: String) = Error(s"Table $dbname already exists")
	final def TABLE_DOES_NOT_EXISTS(dbname: String) = Error(s"Table $dbname doesn't exists")
	final def DATABASE_NOT_SELECTED = Error(s"No database is selected")
	final def THE_IMPOSSIBLE_HAPPENED(s: String) = Error(s"The impossible happened at $s")
	final def COLUMN_EXISTS(colname: String) = Error(s"Column $colname exists")
	final def CONSTRAINT_EXISTS(tablename: String, constraintname: String) = Error(s"Constraint $constraintname already exists on table $tablename")
	final def CONSTRAINT_DOES_NOT_EXISTS(tablename: String, constraintname: String) = Error(s"Constraint $constraintname doesn't exists on table $tablename")
	final def COLUMN_DOES_NOT_EXISTS(colname: String) = Error(s"Table $colname doesn't exists")
	final def COMMAND_CANCELED = Error(s"Command canceled!")

	def exec(cmd: Command):ExecutionResult = cmd match {
	  case CreateDatabase(dbname) => {
	    Databases.create(dbname).fold[ExecutionResult](DATABASE_ALREADY_EXISTS(dbname))(_ => AffectedRows(1))
	  }

	  case CreateTable(tbName, columns, constraints) => {
	    val maybeTable = Tables.create(tbName, columns, constraints)
	    maybeTable match {
	      case Left(e) => e
	      case _ => AffectedRows(1)
	    }
	  }

	  case DropTable(tbName) => Databases.current match {
	    case Some(db) => Tables.drop(db, tbName) match {
	      case Left(e) => e
	      case _ => AffectedRows(1)
	    }
	    case None => Executor.DATABASE_NOT_SELECTED
	  }

	  case ShowDatabases() => {
	    val currentDBName = Databases.current.fold("")(_.name)

	    val dbNames = Databases.dbList.map(db => {
	    	if (db.name == currentDBName)
	    	  db.name + " [*]"
	    	else
    	      db.name
	    })

	    println(dbNames.mkString("\n"))
	    AffectedRows(0)
	  }

	  case ShowTables() => {

	    val maybeTables = Tables.tbList
	    maybeTables match {
	      case Left(e)=> e
	      case Right(theList)=> {
	        val tbNames = theList.map(db => db.name)
		    println(tbNames.mkString("\n"))
		    AffectedRows(0)
	      }
	    }
	  }

	  case ShowColumns(tableName) => {

	    val maybeCols = Tables.getCols(tableName)
	    maybeCols match {
	      case Left(e) => e
	      case Right(colList) => {
	        val colNames = colList.map(col => col.name + " ("+col.typ.toString()+")")
	        println(colNames.mkString("\n"))
	        AffectedRows(0)
	      }
	    }
	  }

	  case DropDatabase(dbname) => {
	    Console.print("Are you sure you want to drop? (y/n): ")
	    Console.flush();
	
	    val buffer = new BufferedReader(new InputStreamReader(System.in));
	    var input = buffer.readLine();
	    if ((input.equals("y"))){
	    val success = AffectedRows(1)
	    Databases.drop(dbname).fold[ExecutionResult](DATABASE_DOES_NOT_EXIST(dbname))(_ => success)
	     } else (Executor.COMMAND_CANCELED)
	  }

	  case RenameTable(tbName, newName) => Tables.rename(tbName, newName) match{
	    case Right(db) => AffectedRows(1)
	    case Left(e) => e
	  }

	  case UseDatabase(dbname) => Databases.use(dbname) match{
	    case Some(db) => AffectedRows(0)
	    case _ => DATABASE_DOES_NOT_EXIST(dbname)
	  }

	  case AlterDatabase(oldName, newName) => Databases.alter(oldName, newName) match{
	    case Right(db) => AffectedRows(1)
	    case Left(e) => e
	  }

	  case AddColumn(tableName, columnDefinition, constraints) => Tables.addColumn(tableName, columnDefinition, constraints) match{
	    case Right(table) => AffectedRows(1)
	    case Left(e) => e
	  }

	  case DropColumn(tableName, columnName) => {
	    Console.print("Are you sure you want to drop? (y/n): ")
	    Console.flush();
	
	    val buffer = new BufferedReader(new InputStreamReader(System.in));
	    var input = buffer.readLine();
	    if ((input.equals("y"))){
		    Tables.dropColumn(tableName, columnName) match{
			    case Right(table) => AffectedRows(1)
			    case Left(e) => e
			  }
	    } else (Executor.COMMAND_CANCELED)
	  }

	  case AddConstraint(tableName, constraints) => Tables.addConstraint(tableName, constraints) match{
	    case Right(table) => AffectedRows(1)
	    case Left(e) => e
	  }

	  case DropConstraint(tableName, columnName) => {
	    Console.print("Are you sure you want to drop? (y/n): ")
	    Console.flush();
	
	    val buffer = new BufferedReader(new InputStreamReader(System.in));
	    var input = buffer.readLine();
	    if ((input.equals("y"))){
		    Tables.dropConstraint(tableName, columnName) match{
	    case Right(table) => AffectedRows(1)
	    case Left(e) => e
		    }
		    } else (Executor.COMMAND_CANCELED)
	  }
	  
	  case Insert(tableName, columnList, values) => Databases.current match {
	    case None => Error("No database is selected.")
	    case Some(db) => Tables.findTableByName(tableName) match {
	      case Some(table) => benchmark(Rows.insertInto(db, table, columnList, values)) match {
	        case Left(e) => e
	        case Right(x) => x
	      }
	      case None => Executor.TABLE_DOES_NOT_EXISTS(tableName)
	    }
	  }
	  
	  case SelectCommand(projections, tablename, predicate, orderby) => Databases.current match {
	    case None => Error("No database is selected.")
	    case Some(db) => Tables.findTableByName(tablename) match {
	      case Some(table) => benchmark(Rows.selectFrom(db, table, projections, predicate, orderby)) match {
	        case Left(e) => e
	        case Right(x) => x
	      }
	      case None => Executor.TABLE_DOES_NOT_EXISTS(tablename)
	    }
	  }
	  
	  case DeleteFROM(tablename, predicate) => Databases.current match {
	    case None => Error("No database is selected.")
	    case Some(db) => Tables.findTableByName(tablename) match {
	      case Some(table) => benchmark(Rows.deleteFrom(db, table, predicate)) match {
	        case Left(e) => e
	        case Right(x) => x
	      }
	      case None => Executor.TABLE_DOES_NOT_EXISTS(tablename)
	    }
	  }
	  
	  case UpdateCommand(tablename, assignments, predicate) => Databases.current match {
	    case None => Error("No database is selected.")
	    case Some(db) => Tables.findTableByName(tablename) match {
	      case Some(table) => benchmark(Rows.updateWhere(db, table, assignments, predicate)) match {
	        case Left(e) => e
	        case Right(x) => x
	      }
	      case None => Executor.TABLE_DOES_NOT_EXISTS(tablename)
	    }
	  }
	  
	  case ReadScript(filename: String) => {
	      val now = (new Date()).getTime()
	      log ("reading file ... ")
	      val script = Filesystem.readFile(filename).lines.toList
	      var after = (new Date()).getTime()
	      log ("... done in "+ (after - now) + "ms")
	      
	      log ("parsing " + script.size + " lines ..")
	      val commands = script.flatMap(Parser.parse)
	      after = (new Date()).getTime()
	      log ("... done in "+ (after - now) + "ms")
	      
	      log ("executing " + commands.size + " commands ...")
	      Rows.flushOnEnd = false;
	      //val results = commands.map(c => if (c.nonEmpty) exec(c.get) else Error("Could not parse"))
	      val results = commands.map(exec)
	      after = (new Date()).getTime()
	      log ("... done in "+ (after - now) + "ms")
	      
	      Rows.flushTables
	      Rows.flushOnEnd = true
	      
	      after = (new Date()).getTime()
	      log ("... done in "+ (after - now) + "ms")
	      ShowRows(Nil,Nil)
	  }
	
	  case c => Error("Not implemented yet: "+c)
	}
	
	def benchmark(f: => Either[Error,ExecutionResult]): Either[Error,ExecutionResult] = {
	  val now = (new Date()).getTime()
	  val res = f
	  val after = (new Date()).getTime()
	  
	  res match {
	    case Left(e) => Left(e)
	    case Right(afrs) => Right(Benchmarked(afrs, after - now))
	  }
	}
	
	
	def parseAndExec(input: String, loop: => Unit): Unit = {
	    val parseResult : Option[Command] = Parser.parse(input)
	    val cmdParseResult = parseResult.toString()
	    
	    parseResult match {
	      case Some(cmd) => {
	        ConsoleHistory.append(input)
	        val result = Executor.exec(cmd)
	        if (doIO) println(result)
	      }
	      case _ => println("Unknown command. " + input + " Try `help`")
	    }
	    
	    if (input != "exit")
		  loop
	  }
}