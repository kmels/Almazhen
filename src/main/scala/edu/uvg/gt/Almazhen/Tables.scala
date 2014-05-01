/**
 * Created by pchang on 5/1/14.
 */
package edu.uvg.gt.Almazhen

import scalaz._, Scalaz._
import argonaut._, Argonaut._
import java.io.File

object Tables {
	final val TB_METAFILE = "tables.meta"
	implicit def TablesJson: CodecJson[Table] = casecodec3(Table.apply, Table.unapply) ("name", "restrictions", "columns")

  /**
   * Creates a new table.
   *
   * If the database exists, it's left intact and None is returned.
   * If user hasn't set the current database, None is returned.
   */
  def create(tbName: String, tbCols: List[String/*ColumnDefinition*/], tbRestrictions: List[String/*Restriction*/]): Option[Table] = {
    val tables = tbList

    if (tables.exists(table => table.name == tbName)){
      //the table already exists
      return None
    }else{      
      if (Databases.current.nonEmpty)  {
	      val newtb = Table(tbName, tbCols, tbRestrictions)
	      val updated_tbs = tables :+ newtb
	      this.setTablesTo(updated_tbs)
	      return Some(newtb)  
      }
      else
        return None
        
    }
  }	
	
  def tbList : List[Table] = Filesystem.readFile(Databases.current.get.name + File.separator + this.TB_METAFILE).decodeOption[List[Table]].getOrElse(Nil)

  def setTablesTo(tables: List[Table]): Unit = {
    val serialized_tbs = tables.asJson.toString
    Filesystem.writeFile(Databases.current.get.name + File.separator + this.TB_METAFILE, serialized_tbs)
  }

  /**
   * Drops an existent table.
   *
   * If the table does not exist, None is returned.
   * Otherwise, the just-dropped database is returned
   */
  def drop(tbname: String): Option[Table] = {
    val tbs = tbList
    val maybeTB = tbs.find(_.name == tbname)
    
    maybeTB.fold[Option[Table]](None)(tb => {
      //delete and return it
      setTablesTo(tbs.filter(_ != tb))
      Some(tb)
    })
  }
  
  
}