package edu.uvg.gt.Almazhen

class ExecutionResult
case class AffectedRows(n: Int) extends ExecutionResult{
  override def toString = "Affected " + n + " rows"
}

case class Error(msg: String) extends ExecutionResult{
  override def toString = msg
}

object Executor {
	final val DATABASE_DOES_NOT_EXIST = Error("Database doesn't exist")
  
	def exec(cmd: Command):ExecutionResult = cmd match {
	  case CreateDatabase(dbname) => {
	    val e = Error("Database exists")
	    val success = AffectedRows(1)
	    Databases.create(dbname).fold[ExecutionResult](e)(_ => success)
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
	  
	  case DropDatabase(dbname) => {
	    val success = AffectedRows(1)
	    Databases.drop(dbname).fold[ExecutionResult](DATABASE_DOES_NOT_EXIST)(_ => success)
	  }
	  
	  case UseDatabase(dbname) => Databases.use(dbname) match{
	    case Some(db) => AffectedRows(0)
	    case _ => DATABASE_DOES_NOT_EXIST
	  }

	  case c => Error("Not implemented yet: "+c)
	}
}