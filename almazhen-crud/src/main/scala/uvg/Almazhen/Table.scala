package uvg.Almazhen

import edu.uvg.gt.Almazhen.ColumnDefinition
import edu.uvg.gt.Almazhen.Databases
import edu.uvg.gt.Almazhen.Constraint
import edu.uvg.gt.Almazhen._
import edu.uvg.gt.Almazhen.{Table, Tables, IntType, Pk_key}

/**
 * Created by kmels on 23/05/14.
 */
object DBTable {
  Databases.create("CRUD")
  Databases.use("CRUD")

  final val initialColumns: List[ColumnDefinition] = List(
    ColumnDefinition("id", IntType)
  )

  final val columnConstraints: List[Constraint] = List(
    Pk_key("id", List("id"))
  )

  final val table_name = "Personas"

  private def makeTable: Table = Tables.create(table_name, initialColumns, Nil) match{
    case Left(err) => throw (new Exception(err.toString()))
    case Right(t) => t
  }

  private def table: Table = Tables.findTableByName(table_name) match{
    case Some(t) => t
    case _ => makeTable
  }

  def getTableColumns: List[ColumnDefinition] = table.columns
}
