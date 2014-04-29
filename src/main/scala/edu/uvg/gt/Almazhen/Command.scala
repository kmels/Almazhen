package edu.uvg.gt.Almazhen

abstract class Command

case class CreateDatabase(databaseName : String) extends Command

//TODO
case class AlterDatabase(name : String,newName : String) extends Command

//TODO
case class DropDatabase(databaseName : String) extends Command

//TODO
case class ShowDatabases() extends Command

//TODO
case class UseDatabase(databaseName : String) extends Command

//TODO
case class CreateTable(name : String, Columns : List[ColumnDefinition], Constraints: List[Restriction]) extends Command

//TODO
case class RenameTable(name : String, newName : String) extends Command

//TODO
case class AddColumn(tableName : String, columnSpecification : ColumnDefinition, Constraints: List[Restriction]) extends Command

//TODO
case class AddConstraint(tableName : String, constraint: Restriction) extends Command

//TODO
case class DropColumn(tableName : String, columnName: String) extends Command

//TODO
case class DropConstraint(tableName : String, constraintName: String) extends Command

//TODO
case class DropTable(tableName : String) extends Command

//TODO
case class ShowTables() extends Command

//TODO
case class ShowColumns(tableName : String) extends Command

//TODO
case class Insert(tableName : String, values : List[String]) extends Command

//TODO
case class InsertWithColumns(tableName : String, columns : List[String], values : List[String]) extends Command

//TODO
case class UpdateCommand(tableName : String, columnValuePair : List[Assignment], check : Predicate) extends Command

//TODO
case class Delete(tableName : String, check : Predicate) extends Command

//TODO
case class SelectCommand(projections : List[String], tableName : String, check : Predicate, order : List[OrderBy] = List()) extends Command

//TODO
case class SelectAll(tableName : String, check : Predicate, order : OrderBy) extends Command
