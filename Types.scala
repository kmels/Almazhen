/**
 * Created by kmels on 3/20/14.
 */

import java.io.{InputStreamReader, BufferedReader}
import java.util.Scanner;

class ColumnSpec() {
}

class Predicate() {}

class OrderBy() {}

class AZtype[+T](implicit m:Manifest[T]) {
  override def toString = "AZtype["+m.toString+"]"
}

object ColumnInt extends ColumnSpec
object ColumnFloat extends ColumnSpec
object ColumnDate extends ColumnSpec
object ColumnChar extends ColumnSpec

class Pk_key() {}
class Fk_key() {}
class Ch_key() {}

class ColValuePair(colName : String, colValue : String ){}
class OrderExpr(expression : String, order : String){}

class Restriction() {}