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

	private def findByName(tbname: String): Either[Error, Table] = {
	  val maybeTables = tbList 
	  maybeTables match {
		
		  case Left(e) => Left(e)
		  
		  case Right(tableList) => {
		    val maybeTable = tableList.find(_.name == tbname)
		    if (maybeTable.isEmpty)
		      Left(Executor.TABLE_DOES_NOT_EXISTS(tbname))
	      else
	        Right(maybeTable.get)
		  }
	  }
	}
	
  /**
   * Creates a new table.
   *
   * If the database exists, it's left intact and None is returned.
   * If user hasn't set the current database, None is returned.
   */
  def create(tbName: String, tbCols: List[ColumnDefinition], tbRestrictions: List[Constraint]): Either[Error, Table] = {
    
    val maybeTables = tbList 
    
    maybeTables match {
      case Left(e) => Left(e)
      
      case Right(tableList) => {
        if (tableList.exists(table => table.name == tbName))
        {
          return Left(Executor.DATABASE_ALREADY_EXISTS(tbName))
        }  
        else {      	      
	      val newtb = Table(tbName, tbCols, tbRestrictions)
	      val updated_tbs = tableList :+ newtb
	      this.setTablesTo(updated_tbs)
	      return Right(newtb)
        }
      }
      
    }
    
  }	
	
  def tbList : Either[Error,List[Table]] = {
    val maybeTB = Databases.current 
    
    if (maybeTB.isEmpty)
      Left(Executor.DATABASE_NOT_SELECTED)
    else {
    	val selectedDb = maybeTB.get  
	    val thefile = Filesystem.readFile(selectedDb.name + File.separator + this.TB_METAFILE)
	    val decodedFile = thefile.decodeOption[List[Table]]
	    val getfile = decodedFile.getOrElse(Nil)
	    
	    getfile match{
    	    case Nil => Left(Executor.THE_IMPOSSIBLE_HAPPENED("get table list"))
    	    case _ => Right(getfile)
    	  }
    }
  }
  
  /**
   * Sync table list
   */
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
  def drop(tbname: String): Either[Error,Table] = {
    
    val maybeTB = findByName(tbname)
    
    maybeTB match {
      case Left(e) => Left(e)
      
      case Right(table) => {
        val maybeList = tbList
        maybeList match {
          case Left(e) => Left(e)
      
          case Right(tableList) => {
        	  setTablesTo(tableList.filter(_ != table))
        	  Right(table)
          }
        }
        
      }
    }
    
  }
  
  /**
   * Gets the columns associated with a table
   * 
   * Returns an error if table doesn't exists
   */
  def getCols(tbName: String): Either[Error, List[ColumnDefinition]] = {
    val maybeTB = findByName(tbName)
    maybeTB match {
      case Left(e) => Left(e)
      
      case Right(singleTable) => {
        Right(singleTable.cols)
      }
    }
  }
  
  /**
   * Renames an existent table.
   *
   * If the table does not exist, None is returned.
   * Otherwise, the just-renamed table is returned
   */
  def rename(tbname: String, newName: String): Either[Error, Table] = {
    val maybeOld = findByName(tbname)
	
    maybeOld match {
      case Left(e) => Left(e)
      case Right(oldTB) => {
        tbList match {
          case Left(e) => Left(e)
          case Right(tbs) => {
            val probableConflict = tbs.find(_.name == newName)
            
            if (probableConflict.isEmpty) {
              val updatedtbs = tbs.mapConserve(tb => {
	    	    if (tb.name == tbname){
	    	    	oldTB.copy(name = newName)
	    	    }
		    	else
		    		tb
	    	  })
	    	  
	    	  setTablesTo(updatedtbs)
	    	  findByName(newName) match{
	    	    case Right(alteredDb) => Right(alteredDb)
	    	    case _ => Left(Executor.THE_IMPOSSIBLE_HAPPENED("alter table"))
	    	  }
            }
            else {
              Left(Executor.TABLE_ALREADY_EXISTS(newName))
            }
          }
        }
      }
    }
  }
  
}