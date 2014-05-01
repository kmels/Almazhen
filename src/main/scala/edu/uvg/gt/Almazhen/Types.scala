/**
 * Created by kmels on 3/20/14.
 */
package edu.uvg.gt.Almazhen


import java.io.{InputStreamReader, BufferedReader}
import java.util.Scanner;

case class Database(name: String, table_count: Int)

case class Table(name: String, cols: List[String/*ColumnDefinition*/], restrictions: List[String/*Constraint*/])

case class ColumnDefinition(name: String, typ: AZtype)

case class Predicate

case class OrderBy(expression : String, order : String)

abstract class AZtype

object IntType extends AZtype
object FloatType extends AZtype
object DateType extends AZtype
object CharType extends AZtype

class Constraint
case class Pk_key(name: String, cols: List[String]) extends Constraint

case class Fk_key(name: String, cols: List[(String,String)]) extends Constraint

case class Ch_key(name: String, check: Predicate) extends Constraint

case class Assignment(columnName : String, value : String )
