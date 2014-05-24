package edu.uvg.gt.Almazhen

abstract class Command

case class CreateDatabase(databaseName: String) extends Command

case class AlterDatabase(name: String, newName: String) extends Command

case class DropDatabase(databaseName: String) extends Command

case class ShowDatabases extends Command

case class ReadScript(filename: String) extends Command

case class UseDatabase(databaseName : String) extends Command

case class CreateTable(name : String, Columns : List[ColumnDefinition], Constraints: List[Constraint]) extends Command

case class RenameTable(name : String, newName : String) extends Command

case class AddColumn(tableName : String, columnSpecification : ColumnDefinition, Constraints: List[Constraint]) extends Command

case class AddConstraint(tableName : String, constraint: Constraint) extends Command

case class DropColumn(tableName : String, columnName: String) extends Command

case class DropConstraint(tableName : String, constraintName: String) extends Command

case class DropTable(tableName : String) extends Command

case class ShowTables() extends Command

case class ShowColumns(tableName : String) extends Command

case class Insert(tableName : String, columnList:Option[List[String]], values : List[String]) extends Command

//TODO
//case class InsertWithColumns(tableName : String, columns : List[String], values : List[String]) extends Command

//TODO
case class UpdateCommand(tableName : String, columnValuePair : List[Assignment], check : Option[Predicate]) extends Command

//TODO
case class DeleteFROM(tableName : String, check : Option[Predicate]) extends Command

//TODO
case class SelectCommand(projections: Option[List[String]], tableName : String, check : Option[Predicate] = None, order : List[OrderBy] = List()) extends Command

//TODO
//case class SelectAll(tableName : String, check : Predicate, order : OrderBy) extends Command
