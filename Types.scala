/**
 * Created by kmels on 3/20/14.
 */

import java.io.{InputStreamReader, BufferedReader}
import java.util.Scanner;

class ColumnSpec(name: String, ColumnType : AZtype) {
}


class AZtype[+T](implicit m:Manifest[T]) {
  override def toString = "AZtype["+m.toString+"]"
}

object columnInt extends AZtype[Boolean]
object columnBool extends AZtype[Int]
object columnChar extends AZtype[Char]

class Restriction(expression: String)