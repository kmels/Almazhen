
class Command

case class CreateDatabase(databaseName : String) extends Command {
  def execute = {

  }
}

case class AlterDatabase(name : String,newName : String) extends Command {}


case class DropDatabase(databaseName : String) extends Command {
  def execute = {

  }
}
case class ShowDatabases() extends Command {}

case class UseDatabase(databaseName : String) extends Command {
  def execute = {

  }
}