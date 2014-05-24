package uvg.Almazhen

import edu.uvg.gt.Almazhen.ColumnDefinition
import edu.uvg.gt.Almazhen.{Databases, Database}
import edu.uvg.gt.Almazhen.{Constraint, Rows, ShowRows, Row}
import edu.uvg.gt.Almazhen._
import edu.uvg.gt.Almazhen.{Table, Tables, DateType, IntType, FloatType, Pk_key}

/**
 * Created by kmels on 23/05/14.
 */
object DBTable {
  Databases.create("CRUD")
  Databases.use("CRUD")

  def db: Database = Databases.use("CRUD") match{
    case None => Databases.create("CRUD").fold[Database](null)(db => db)
    case Some(db) => db
  }

  final val initialColumns: List[ColumnDefinition] = List(
    ColumnDefinition("id", IntType),
    ColumnDefinition("nombre", VARCHAR(20)),
    ColumnDefinition("apellido", VARCHAR(20)),
    ColumnDefinition("credito", FloatType),
    ColumnDefinition("nacimiento", DateType)
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

  def remake: Table = {
    println("Borrando tabla ...")
    Tables.drop(db, table_name)
    println("Creando tabla ...")
    this.makeTable
  }

  def insert(values: List[String]) = Rows.insertInto(db, table, None, values)

  def values:List[List[String]] = Rows.selectFrom(db, table, None, None, Nil) match{
    case Right(ShowRows(colnames, rows)) => rows.map(row => row.values.map(_.column_value))
    case Left(err) => {
      println("Error: "+err.toString())
      Nil
    }
    case _ => Nil //exhaustive match
  }

  def getTableColumns: List[ColumnDefinition] = table.columns
}
