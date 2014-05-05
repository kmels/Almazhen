package edu.uvg.gt.Almazhen

import java.text.SimpleDateFormat
import java.text.ParseException

import scalaz._, Scalaz._
import argonaut._, Argonaut._

import java.util.Date

case class ColumnValue(column_name: String, column_value: String){
  override def toString = column_value
}

case class Row(values: List[ColumnValue]){
  var projection: List[String] = List()
  
  override def toString = {
    val vals: List[String] = values.flatMap((v => if (!projection.contains(v.column_name)) None else Some(v.column_value)))
    vals.mkString("\t")
  }
  
  def setProjection(ps: List[String]): Unit = projection = ps
}

object Rows{
  def log(s: String) = {} //println(s)
  
  /*
   * Type checks a value string.
   * 
   * retuns true if, and only if, the value is coercable to the given type
   */
  def coercesTo(value: String, type_ : AZtype): Boolean = type_ match{
    case IntType => try { val a = value.toInt; return true } catch{case e: NumberFormatException => false}
    case FloatType => try { value.toFloat; return true } catch{case e: NumberFormatException => false}
    case VARCHAR(size) => value.size < size
    case DateType => try { val df = new SimpleDateFormat("yyy-MM-dd"); df.parse(value); return true } catch { case e: ParseException => false}
  }
  
  def insertInto(db: Database, table: Table, columnList:Option[List[String]], values: List[String]):Either[Error,ExecutionResult] = {
    log("Inserting into "+table.name)
        
    //make the column list
    val maybeColumns:Either[Error,List[ColumnDefinition]] = columnList match {
      case None => Right(table.columns)
      case Some(List()) => Right(table.columns) //empty list
      case Some(colnames) => {
        println("There is a column list: "+colnames)
    	  //if some column name doesn't exist, return error
    	  if (! colnames.forall(colname => table.columns.exists(_.name == colname))){
    	    val colname = colnames.diff(table.columns.map(cl => cl.name)).head
    	    Left(Error("Column named $colname colname doesn't exist"))
    	  }else
    	    //all columns exist
    	    Right(colnames.map(colname => table.columns.find(_.name == colname).get)) 	 
      }
    }
    
    if (maybeColumns.isRight)
    	log("column order is: " + maybeColumns.right.get.map(_.name))
    
    //typed instruction
    val typedColumns: Either[Error, List[(ColumnDefinition,ColumnValue)]] = maybeColumns.fold(Left(_), columns => {
    	if (columns.size != values.size)
    	  Left(Error("The number of values doesn't match the number of columns, expecting: "+columns.size))
    	else{
    	  val zipped = columns.zip(values) 
    	  
    	  //number of columns and values match
    	  //type check every column value
    	  val typeChecks = zipped.forall{case (coldef,val_) => coercesTo(val_, coldef.typ)}
    	  if (!typeChecks){
    	    val prob: (ColumnDefinition, String) = zipped.find({ case (c,v) => !coercesTo(v,c.typ)}).get
    	    Left(Error("The value "+prob._2.toString() + " is not of the type of "+prob._1.name))
    	  }else{
    	    val colvals: List[ColumnValue] = zipped.map{ case (coldef, val_) => ColumnValue(coldef.name, val_)}
    	    Right(columns.zip(colvals))
    	  }
    	}
    })
   
    //do insert
    if (typedColumns.isLeft)
      return Left(typedColumns.left.get)
    else{
    	val columnVals: List[ColumnValue] = typedColumns.right.get.map(_._2)
    	val row: Row = Row(columnVals)
    	val rows: List[Row] = getRows(db,table) :+ row
    	writeRows(db, table, rows)
    	Right(AffectedRows(1))
    }
  }
  
  def selectFrom(db: Database, table: Table, projections: Option[List[String]], predicate: Option[Predicate], orderbyList: List[OrderBy]): Either[Error,ExecutionResult] = {
    log("selecting from "+table.name)
    
    val projectColumns: List[String] = projections.fold[List[String]](table.columns.map(_.name))(xs => xs)
       
    val rows:List[Row] = predicate match{
      case None => getRows(db, table)
      case Some(p) => List()
    }
    
    log("Ordering by fields: "+ orderbyList)
    val orderedRows: List[Row] = orderRows(table, orderbyList, rows)
    
    log("Setting projection list to "+projectColumns.mkString(","))
    
    orderedRows.foreach(_.setProjection(projectColumns))
    
    
    Right(ShowRows(projectColumns, orderedRows))
  }  
  
  object RowOrdering extends scala.math.Ordering[(Option[ColumnDefinition], Option[ColumnValue])] {
    val df = new SimpleDateFormat("yyyy-MM-dd")
    
	  def compare(a: (Option[ColumnDefinition], Option[ColumnValue]), b:(Option[ColumnDefinition], Option[ColumnValue])) = {
      println("Comarping "+ a._2.get + " to "+b._2.get)
        a._1 match{
	    case None => 0
	    case Some(coldef) => a._2 match {
	      case None => 0
	      case Some(ColumnValue(_, x)) => coldef.typ match{
	        case IntType => { 
	          val z= x.toInt compare b._2.get.column_value.toInt
	          println("ya man" + z)
	          z} 
	        case FloatType => x.toFloat compare b._2.get.column_value.toFloat
	        case DateType => df.parse(x).getTime() compare df.parse(b._2.get.column_value).getTime()
	        case _ => x compare b._2.get.column_value
	      }
	    }
	  }
    }
  }
  
  implicit def rowOrdering = RowOrdering
  
  def orderRows(table: Table, order: List[OrderBy], rows: List[Row]): List[Row] = order match{
    case Nil => rows
    case (head :: tail) => {
    	log ("Ordering " + rows.size + " rows by "+head.expression)
    	val columnDef = table.columns.find(_.name == head.expression)
    	
    	val sorted = rows.sortBy(row => {
    		val columnVal = row.values.find(v => v.column_name == head.expression)
    		(columnDef,columnVal)
    	})
    	
    	sorted.foreach(_.setProjection(order.map(_.expression)))
    	println("Sorted!")
    	println(sorted)
    	if (head.orderDirection == ASCOrder)
    		orderRows(table, tail , sorted)
		else
		    orderRows(table, tail, sorted.reverse)
    }
  }
  
  def deleteFrom(db: Database, table: Table, predicate: Option[Predicate]): Either[Error, AffectedRows] = predicate match {
    case None => {
      val rows = getRows(db, table) 
      writeRows(db, table, List()) //delete all
      Right(AffectedRows(rows.size))
    }
    case Some(p) => {
      val rows = getRows(db, table)
      writeRows(db, table, List())
      Right(AffectedRows(rows.size))
    }
  }
  
  def writeRows(db: Database, table: Table, rows: List[Row]) = Filesystem.writeFile(db.name + "/" + table.name + ".data", rows.asJson.toString)
  
  def getRows(db:Database, table: Table): List[Row] = Filesystem.readFile(db.name + "/" + table.name + ".data").decodeOption[List[Row]].getOrElse(Nil)
 // def dbList : List[Database] = Filesystem.readFile(this.DB_METAFILE).decodeOption[List[Database]].getOrElse(Nil)

  implicit def RowJson: CodecJson[Row] = casecodec1(Row.apply, Row.unapply)("values")
  implicit def ColumnValueJson: CodecJson[ColumnValue] = casecodec2(ColumnValue.apply, ColumnValue.unapply)("column_name", "column_value")
}


