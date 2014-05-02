/**
 * Created by pchang on 5/1/14.
 */
package edu.uvg.gt.Almazhen

import scalaz._, Scalaz._
import argonaut._, Argonaut._
import java.io.File

object Tables {
	final val TB_METAFILE = "tables.meta"
	import Implicits._
	implicit def TablesJson: CodecJson[Table] = casecodec3(Table.apply, Table.unapply) ("name", "columns", "restrictions")

  /**
   * Creates a new table.
   *
   * If the database exists, it's left intact and None is returned.
   * If user hasn't set the current database, None is returned.
   */
  def create(tbName: String, tbCols: List[ColumnDefinition], tbRestrictions: List[Constraint]): Option[Table] = {
    if (Databases.current.nonEmpty)  {
	  
    	val tables = tbList
	    if (tables.exists(table => table.name == tbName)){
	      //the table already exists
	      return None
	    }else{      
	      
		      val newtb = Table(tbName, tbCols, tbRestrictions)
		      val updated_tbs = tables :+ newtb
		      this.setTablesTo(updated_tbs)
		      return Some(newtb)
	    }
    } else { 
      return None
    }
  }	
	
  def tbList : List[Table] = {
    val thefile = Filesystem.readFile(Databases.current.get.name + File.separator + this.TB_METAFILE)
    val decodedFile = thefile.decodeOption[List[Table]]
    val getfile = decodedFile.getOrElse(Nil)
    return getfile
  }

  def setTablesTo(tables: List[Table]): Unit = {
    val serialized_tbs = tables.asJson.toString
    Filesystem.writeFile(Databases.current.get.name + File.separator + this.TB_METAFILE, serialized_tbs)
  }

  /**
   * Drops an existent table.
   *
   * If the table does not exist, None is returned.
   * Otherwise, the just-dropped table is returned
   */
  def drop(tbname: String): Option[Table] = {
    val route = Databases.current.get.name + File.separator + this.TB_METAFILE
    val tbs = tbList
    val maybeTB = tbs.find(_.name == tbname)
    
    maybeTB.fold[Option[Table]](None)(tb => {
      //delete and return it
      setTablesTo(tbs.filter(_ != tb))
      Some(tb)
    })
  }
  
  /**
   * Renames an existent table.
   *
   * If the table does not exist, None is returned.
   * Otherwise, the just-renamed table is returned
   */
  def rename(tbname: String, newName: String): Option[Table] = {
    val route = Databases.current.get.name + File.separator + this.TB_METAFILE
    val tbs = tbList
    val maybeTB = tbs.find(_.name == tbname)
    maybeTB match {
      case Some(t) => {
        setTablesTo(tbs.map {case t => t.copy(name = newName)})
        Some(t.copy(name = newName))
      }
      case None => None
    }
  }
  
  /**
   * Finds table by name
   */
  def findByName(tablename: String) = {
    println(tbList)
    println(tbList.map(_.name))
    tbList.find(_.name == tablename)
  }
  
  /**
   * Adds a new column to a table
   */
  def addColumn(tablename: String, newcolumn: ColumnDefinition, newconstraints: List[Constraint]): Either[ExecutionResult, Table] = 
    Databases.current  match{
    	case Some(currentDB) => findByName(tablename) match{
    		case Some(table) => {
    			//check that the table doesn't contain the column already
    		    if (table.columns.exists(_.name == newcolumn.name))
    		      Left(Executor.COLUMN_EXISTS(newcolumn.name))
    		    else {
    		      //check that the table doesn't contain the contraint already
    		      val oldies = table.restrictions.map(_.name)
    		      val news = newconstraints.map(_.name)
    		      
    		      if (oldies.intersect(news).size > 0)
    		        Left(Executor.CONSTRAINT_EXISTS(table.name, newcolumn.name))
    		      else{
    		        val new_tables: List[Table] = tbList.map(t => {
    		    		if (t.name == table.name){
    		    		  t.copy(columns = t.columns :+ newcolumn, restrictions = t.restrictions ++ newconstraints)
    		    		}else
    		    		  t
    		        })
    			
    		        setTablesTo(new_tables)
    		        Right(table)
    		      }
    		    }
    		}
    		case None => Left(Executor.TABLE_DOES_NOT_EXISTS(tablename))
    	} 
    	case _ => Left(Error("No database is selected."))
  	} 
}