package edu.uvg.gt.Almazhen

import java.text.SimpleDateFormat
import java.text.ParseException

import scalaz._, Scalaz._
import argonaut._, Argonaut._

case class ColumnValue(column_name: String, column_value: String){
  override def toString = column_value
}

case class Row(values: List[ColumnValue]){
  override def toString = values.mkString("\t")
}

object Rows{
  def log(s: String) = println(s)
  
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
    
    
    log("column order is: " + maybeColumns)
    
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
    	    Left(Error("The value "+prob._2 + " is not of the type of "+prob._1.name))
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
  
  def selectFrom(db: Database, table: Table, projections: Option[List[String]], predicate: Option[Predicate], orderby: List[OrderBy]): Either[Error,ExecutionResult] = {
    log("selecting from "+table.name)
    
    val projectToColumns: List[String] = projections.fold[List[String]](table.columns.map(_.name))(xs => xs)
    
    val rows = getRows(db, table)
    
    Right(ShowRows(projectToColumns, rows))
  }
  
  def writeRows(db: Database, table: Table, rows: List[Row]) = Filesystem.writeFile(db.name + "/" + table.name + ".data", rows.asJson.toString)
  
  def getRows(db:Database, table: Table): List[Row] = Filesystem.readFile(db.name + "/" + table.name + ".data").decodeOption[List[Row]].getOrElse(Nil)
 // def dbList : List[Database] = Filesystem.readFile(this.DB_METAFILE).decodeOption[List[Database]].getOrElse(Nil)

  implicit def RowJson: CodecJson[Row] = casecodec1(Row.apply, Row.unapply)("values")
  implicit def ColumnValueJson: CodecJson[ColumnValue] = casecodec2(ColumnValue.apply, ColumnValue.unapply)("column_name", "column_value")
}


