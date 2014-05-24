/**
 * Created by kmels on 3/20/14.
 */
package edu.uvg.gt.Almazhen


import java.io.{InputStreamReader, BufferedReader}
import java.util.Scanner
import scalaz._
import argonaut._

import scalaz._, Scalaz._
import argonaut._, Argonaut._

import StringUtils._
import java.text.SimpleDateFormat
import java.text.ParseException
import java.util.Date

// CODE copied from: https://coderwall.com/p/lcxjzw
object StringUtils {
     implicit class StringImprovements(val strExp: String) {
         import scala.util.control.Exception._
         
         def forceToInt = catching(classOf[NumberFormatException]) opt strExp.toInt
         
         def toIntOpt(implicit row: Row) = row.values.find(_.column_name == strExp) match{
           //the row has the column `str`
           case Some(colVal) => colVal.column_value.forceToInt
           case _ => strExp.forceToInt
         }
         
         def toFloatOpt(implicit row: Row): Option[Float] = row.values.find(_.column_name == strExp) match{
           //the row has the column `str`
           case Some(colVal) => catching(classOf[NumberFormatException]) opt colVal.column_value.toFloat
           case _ => catching(classOf[NumberFormatException]) opt strExp.toFloat
         }
         
         def toDateOpt(implicit row: Row): Option[Date] = row.values.find(_.column_name == strExp) match{
           //the row has the column `str`
           case Some(colVal) => {
             val df = new SimpleDateFormat("yyyy-MM-dd"); 
             catching(classOf[ParseException]) opt (df.parse(colVal.column_value))
           }
           case _ => {
             val df = new SimpleDateFormat("yyyy-MM-dd"); 
             catching(classOf[ParseException]) opt (df.parse(strExp))
           }
         }
         
         def toStringExpr(implicit row: Row) = row.values.find(_.column_name == strExp) match{
           //the row has the column `str`
           case Some(colVal) => colVal.column_value
           case _ => strExp
         }
     }
}

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
    //TODO: Fix this case (Predicate can't be initialized)
    case "CH" => Pk_key(name, Nil)
  });
}

//TODO: Implement compare 
abstract class Predicate
{
  def eval(implicit row:Row): Option[Boolean]
  def compare(exp2: Predicate)(implicit row:Row): Option[Int] 
}

case class ExpressionAnd(expression1: Predicate, expression2: Predicate) extends Predicate {
	override def eval(implicit row:Row): Option[Boolean] = {
	  val maybeExp1 = expression1.eval
	  val maybeExp2 = expression2.eval
	  if (!maybeExp1.isEmpty && !maybeExp1.isEmpty) 
	    Some(maybeExp1.get && maybeExp2.get)  
	  else None
	}
	  
	override def compare(exp: Predicate)(implicit row:Row) = None
}

case class ExpressionOr(expression1: Predicate, expression2: Predicate) extends Predicate {
	override def eval(implicit row:Row): Option[Boolean] = {
	  val maybeExp1 = expression1.eval
	  val maybeExp2 = expression2.eval
	  if (maybeExp1.isDefined && maybeExp1.isDefined)
	    Some(maybeExp1.get || maybeExp2.get)  
	  else None
	}
	
	override def compare(exp: Predicate)(implicit row: Row) = None
}

// <=
case class ExpressionLessOrEquals(expression1: Predicate, expression2: Predicate) extends Predicate {
	override def eval(implicit row:Row): Option[Boolean] = {
	  val maybeCompare = expression1 compare expression2
	  if (maybeCompare.isDefined)
		Some(maybeCompare.get <= 0)
	  else 
	    None
	}
	
	override def compare(exp: Predicate)(implicit row:Row) = None
}
case class ExpressionLess(expression1: Predicate, expression2: Predicate) extends Predicate{
	override def eval(implicit row:Row): Option[Boolean] = {
	  val maybeCompare =expression1 compare expression2
	  if (maybeCompare.isDefined)
	  Some(maybeCompare.get < 0)
	  else None
	}
	
	override def compare(exp: Predicate)(implicit row:Row) = None
}
case class ExpressionGreater(expression1: Predicate, expression2: Predicate) extends Predicate {
	override def eval(implicit row:Row): Option[Boolean] = {
	  val maybeCompare =expression1 compare expression2
	  if (maybeCompare.isDefined)
	  Some(maybeCompare.get > 0)
	  else None
	}
	
	override def compare(exp: Predicate)(implicit row:Row) = None
}
case class ExpressionGreaterOrEquals(expression1: Predicate, expression2: Predicate) extends Predicate {
	override def eval(implicit row:Row): Option[Boolean] = {
	  val maybeCompare =expression1 compare expression2
	  if (maybeCompare.isDefined)
	  Some(maybeCompare.get >= 0)
	  else None
	}
	
	override def compare(exp: Predicate)(implicit row:Row) = None
}
case class ExpressionEquals(expression1: Predicate, expression2: Predicate) extends Predicate {
	override def eval(implicit row:Row): Option[Boolean] = {
	  val maybeCompare =expression1 compare expression2
	  if (maybeCompare.isDefined)
	  Some(maybeCompare.get == 0)
	  else None
	}
	
	override def compare(exp: Predicate)(implicit row:Row) = None
}
case class ExpressionNotEquals(expression1: Predicate, expression2: Predicate) extends Predicate{
	override def eval(implicit row:Row): Option[Boolean] = {
	  val maybeCompare =expression1 compare expression2
	  if (maybeCompare.isDefined)
		  Some(maybeCompare.get != 0)
	  else None
	}
	
	override def compare(exp: Predicate)(implicit row:Row) = None
}

case class NotExpression(expression: Predicate) extends Predicate {
	override def eval(implicit row:Row): Option[Boolean] = {
	 val maybeExp1 = expression.eval
	  if (maybeExp1.isDefined) 
	    Some(!maybeExp1.get)  
	  else None
	}
	
	override def compare(exp: Predicate)(implicit row:Row) = None
}

case class NumericExpressionWrap(expression: String) extends ValueLiteral(expression){
  def intValue(implicit row: Row): Option[Int] = expression.toIntOpt
  
  override def compare(exp: Predicate)(implicit row: Row) = exp match {
    case NumericExpressionWrap(numeric_exp) => {
      val x = this.intValue
      val y = numeric_exp.toIntOpt
      
      //println("Comparing "+ x + " to " + y)
      if (x.isDefined && y.isDefined){
        //println(" ... => "+ Integer.compare(x.get, y.get))
    	Some(Integer.compare(x.get, y.get))
      }
       else None
    }
    case _ => None
  }
  
  //0 is false, otherwise true
  override def eval(implicit row:Row) :Option[Boolean]= intValue.map(_ != 0 )
}

abstract class ValueLiteral(exp: String) extends Predicate{
  override def toString = exp
}

case class FloatExpressionWrap(exp: String) extends ValueLiteral(exp) {
  def floatValue(implicit row: Row): Option[Float] = exp.toFloatOpt
  
  override def eval(implicit row:Row) :Option[Boolean]= floatValue.map(_ => false)
  
  override def compare(exp: Predicate)(implicit row: Row) = ???
}

case class DateExpressionWrap(exp: String) extends ValueLiteral(exp){
  override def compare(exp: Predicate)(implicit row: Row) = ???
  override def eval(implicit row:Row) :Option[Boolean] = ???
}

case class StringExpressionWrap(expression: String) extends ValueLiteral(expression){
  override def compare(exp: Predicate)(implicit row: Row) = exp match{
    case StringExpressionWrap(strExpr) => {
    	val x = expression.toStringExpr
    	val y = strExpr.toStringExpr
    	Some(x.compareTo(y))
    }
    case NumericExpressionWrap(numeric_exp) => {
      val x = this.expression.toIntOpt
      val y = numeric_exp.toIntOpt
      
      //println("Comparing "+ x + " to " + y)
      if (x.isDefined && y.isDefined){
        //println(" ... => "+ Integer.compare(x.get, y.get))
    	Some(Integer.compare(x.get, y.get))
      }
       else None
    }
    case FloatExpressionWrap(numeric_exp) => {
      val x = this.expression.toFloatOpt
      val y = numeric_exp.toFloatOpt
      
      //println("Comparing "+ x + " to " + y)
      if (x.isDefined && y.isDefined){
    	Some(x.get.compareTo(y.get))
      }
       else None
    }
    case _ => None
  }
    
  override def eval(implicit row:Row):Option[Boolean] = None
}


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

case class Assignment(columnName : String, value : ValueLiteral )
