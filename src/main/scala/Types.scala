/**
 * Created by kmels on 3/20/14.
 */

import java.io.{InputStreamReader, BufferedReader}
import java.util.Scanner;

class ColumnSpec() {
}

case class Predicate

case class OrderBy(expression : String, order : String)

class AZtype[+T](implicit m:Manifest[T]) {
  override def toString = "AZtype["+m.toString+"]"
}

object ColumnInt extends ColumnSpec
object ColumnFloat extends ColumnSpec
object ColumnDate extends ColumnSpec
object ColumnChar extends ColumnSpec

class Constraint
case class Pk_key(name: String, cols: List[String]) extends Constraint

case class Fk_key(name: String, cols: List[(String,String)]) extends Constraint

case class Ch_key(name: String, check: Predicate) extends Constraint

case class Assignment(columnName : String, value : String )

case class Restriction()