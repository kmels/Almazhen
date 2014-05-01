/**
 * Created by kmels on 3/20/14.
 */
package edu.uvg.gt.Almazhen


import java.io.{InputStreamReader, BufferedReader}
import java.util.Scanner;
import scalaz._, Scalaz._
import argonaut._, Argonaut._

case class Database(name: String, table_count: Int)

case class Table(name: String, cols: List[ColumnDefinition], restrictions: List[Constraint])

case class ColumnDefinition(name: String, typ: AZtype) 

object Implicits { 
  //implicit def columnDefListJson: EncodeJson[List[ColumnDefinition] = Json.array(elements)
  
  implicit def ColumnDefinitionsJson : CodecJson[ColumnDefinition] = casecodec2(ColumnDefinition.apply, ColumnDefinition.unapply)("name","type")
  implicit def AZtypeEncodeJson: EncodeJson[AZtype] =
    EncodeJson((a: AZtype) =>
      ("theType" := (a match {
        case IntType => "Int"
        case FloatType => "Float"
        case DateType => "Date" 
        case CharType => "Char" 
      }) ) ->: jEmptyObject)
      
  implicit def AZtypeDecodeJson: DecodeJson[AZtype] =
    DecodeJson(c => for {
			      theType <- (c --\ "theType").as[String]
			    }  
		    	yield (theType match{
		    	  case "Int" => IntType
		    	  case "Float" => FloatType
		    	  case "Date" => DateType
		    	  case "Char" => CharType
		    	})
    		)
    
    implicit def ConstraintEncodeJson : EncodeJson[Constraint] = EncodeJson(encodeConstraint)
    implicit def ConstraintDecodeJson : DecodeJson[Constraint] = DecodeJson(decodeConstraint)
    
    def encodeConstraint(a: Constraint): argonaut.Json = a match{
    	case Pk_key(name, cols) => 
    	  (("theType" := "pk_key") ->: ("name" := name) ->: ("cols" := cols) ->: jEmptyObject)
    	case Fk_key(name, cols) => 
    	  (("theType" := "fk_key") ->: ("name" := name) ->: ("cols" := cols) ->: jEmptyObject)
    	case Ch_key(name, check) => 
    	  (("theType" := "ch_key") ->: ("name" := name) ->: ("predicate" := "theCheck") ->: jEmptyObject)  
    }
  
	def decodeConstraint(c: argonaut.HCursor): DecodeResult[Pk_key] = 
	  
	  //return Pk_key((c --\ "name").as[String], (c --\ "cols").as[List[String]])
	  
	  for {
      theType <- (c --\ "theType").as[String]
      name <- (c --\ "name").as[String]
      cols <- (c --\ "cols").as[List[String]]
    } yield ( Pk_key(name, cols) )
	
}

case class Predicate(expression: String)

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
