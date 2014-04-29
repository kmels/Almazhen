package edu.uvg.gt.Almazhen

class ExecutionResult
case class AffectedRows(n: Int) extends ExecutionResult{
  override def toString = "Affected " + n + " rows"
}

case class Error(msg: String) extends ExecutionResult{
  override def toString = msg
}

object Executor {
	def exec(cmd: Command):ExecutionResult = cmd match {
	  case CreateDatabase(dbname) => {
	    val e = Error("Database exists")
	    val success = AffectedRows(1)
	    Databases.create(dbname).fold[ExecutionResult](e)(_ => success)
	  }
	}
}