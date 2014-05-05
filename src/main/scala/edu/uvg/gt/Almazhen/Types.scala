/**
 * Created by kmels on 3/20/14.
 */
package edu.uvg.gt.Almazhen


import java.io.{InputStreamReader, BufferedReader}
import java.util.Scanner;
import scalaz._, Scalaz._
import argonaut._, Argonaut._

case class Database(name: String, table_count: Int)

case class Table(name: String, columns: List[ColumnDefinition], restrictions: List[Constraint])

case class ColumnDefinition(name: String, typ: AZtype) 

object Implicits { 
  implicit def ColumnDefinitionsJson : CodecJson[ColumnDefinition] = casecodec2(ColumnDefinition.apply, ColumnDefinition.unapply)("name","aztype")
  
  /*
   * AZType codec
   */
  implicit def AZToJson: EncodeJson[AZtype] = EncodeJson(atype => atype match {
    case IntType => ("type" := "Int") ->: ("size" := jNumber(0)) ->: jEmptyObject
    case FloatType => ("type" := "Float") ->: ("size" := jNumber(0)) ->: jEmptyObject
    case DateType => ("type" := "Date") ->: ("size" := jNumber(0)) ->: jEmptyObject
    case VARCHAR(size) => ("type" := "Varchar") ->: 
    					  ("size" := jNumber(size)) ->: 
    					  jEmptyObject 
  })
  
  implicit def AZFromJson: DecodeJson[AZtype] = DecodeJson(c => for{
	  typ <- (c --\ "type").as[String]
	  size <- (c --\ "size").as[Int]
  } yield typ match {
    case "Int" => IntType
    case "Float" => FloatType
    case "Date" => DateType
    case "Varchar" => VARCHAR(size)
  })
  
  /**
   * Constraint codec
   */
  implicit def ConstraintToJson: EncodeJson[Constraint] = EncodeJson(constraint => constraint match {
    case Pk_key(name, cols) => ("type" := "PK") ->: 
    						   ("name" := name) ->: 
    						   ("columns" := jArray(cols map {col => jString(col)} )) ->:
    						   ("referenced_table" := "") ->: 
    						   ("column_refs" := jArray(List())) ->: 
    						   ("exp" := "") ->:
    						   jEmptyObject
    						   
    case Fk_key(name, referenced_table, col_refs) => ("type" := "FK") ->: 
    						   						 ("name" := name) ->: 
    						   						 ("columns" := jArray(List())) ->: 
    						   					     ("referenced_table" := referenced_table) ->:
    						   					     ("column_refs" := jArray(col_refs map toRefJson)) ->: 
    						   					     ("exp" := "") ->:
    						   					     jEmptyObject
    						   					     
    case Ch_key(name, predicate) => 
      						  ("type" := "CH") ->:
    						  ("name" := name) ->:
    						  ("columns" := jArray(List())) ->: 
    						  ("referenced_table" := "") ->:
    						  ("column_refs" := jArray(List())) ->: 
    						  ("exp" := predicate.toString) ->:
    						  jEmptyObject
  })
  
  def toRefJson(xs: (String,String)): Json = xs match {
    case (local_col, ref_col) => ("local_col" := local_col) ->: 
    							 ("ref_col"   := ref_col) ->: 
    						     jEmptyObject
  }
  
  implicit def JsonToConstraint: DecodeJson[Constraint] = DecodeJson(c => for {
	typ <- (c --\ "type").as[String]
	name <- (c --\ "name").as[String]
	columns <- (c --\ "columns").as[List[String]]
	referenced_table <- (c --\ "referenced_table").as[String]
	column_refs <- (c --\ "column_refs").as[List[(String,String)]]
	exp <- (c --\ "exp").as[String]
  } yield typ match{
    case "PK" => Pk_key(name, columns)
    case "FK" => Fk_key(name, referenced_table, column_refs)
    case "CH" => Ch_key(name, Predicate(exp))
  });
}

case class Predicate(expression: String)

case class OrderBy(expression : String, orderDirection : OrderByDirection = ASCOrder)

abstract class OrderByDirection
object ASCOrder extends OrderByDirection
object DESCOrder extends OrderByDirection

abstract class AZtype

object IntType extends AZtype
object FloatType extends AZtype
object DateType extends AZtype
case class VARCHAR(size: Int) extends AZtype

abstract class Constraint{
  val name: String
}

case class Pk_key(override val name: String, cols: List[String]) extends Constraint

case class Fk_key(override val name: String, referenced_table: String, cols: List[(String,String)]) extends Constraint

case class Ch_key(override val name: String, check: Predicate) extends Constraint

case class Assignment(columnName : String, value : String )
