
class Command

case class CreateDatabase(databaseName : String) extends Command

case class AlterDatabase(name : String,newName : String) extends Command

case class DropDatabase(databaseName : String) extends Command

case class ShowDatabases() extends Command

case class UseDatabase(databaseName : String) extends Command

case class CreateTable(name : String, Columns : List[ColumnSpec], Constraints: List[Restriction]) extends Command

case class RenameTable(name : String, newName : String) extends Command

case class AddColumn(tableName : String, columnSpecification : ColumnSpec, Constraints: List[Restriction]) extends Command

case class AddConstraint(tableName : String, constraint: Restriction) extends Command

case class DropColumn(tableName : String, columnName: String) extends Command

case class DropConstraint(tableName : String, constraintName: String) extends Command

case class DropTable(tableName : String) extends Command

case class ShowTables() extends Command

case class ShowColumns(tableName : String) extends Command

case class Insert(tableName : String, values : List[String]) extends Command

case class InsertWithColumns(tableName : String, columns : List[String], values : List[String]) extends Command

case class UpdateCommand(tableName : String, columnValuePair : List[Assignment], check : Predicate) extends Command

case class Delete(tableName : String, check : Predicate) extends Command

case class SelectCommand(projections : List[String], tableName : String, check : Predicate, order : List[OrderBy] = List()) extends Command

case class SelectAll(tableName : String, check : Predicate, order : OrderBy) extends Command
