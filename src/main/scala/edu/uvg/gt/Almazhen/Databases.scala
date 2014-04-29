package edu.uvg.gt.Almazhen

import scalaz._, Scalaz._
import argonaut._, Argonaut._

object Databases {
	final val DB_METAFILE = "databases.meta"
	implicit def DatabasesJson: CodecJson[Database] = casecodec2(Database.apply, Database.unapply)("name", "tables_count")
	  
  /**
   * Creates a new database.
   * 
   * If the database exists, it's left intact and None is returned.
   */
  def create(dbname: String): Option[Database] = {
    val databases = dbList
//    println(Filesystem.readFile(this.DB_METAFILE))
    
    if (databases.exists(db => db.name == dbname)){
      //db already exists
      return None
    }else{
      val newdb = Database(dbname, 0)
      val updated_dbs = databases :+ newdb
      this.setDatabasesTo(updated_dbs)
      return Some(newdb)
    }
  }
  
  def dbList : List[Database] = Filesystem.readFile(this.DB_METAFILE).decodeOption[List[Database]].getOrElse(Nil)
  
  def setDatabasesTo(databases: List[Database]): Unit = {
    val serialized_dbs = databases.asJson.toString
    Filesystem.writeFile(this.DB_METAFILE, serialized_dbs)
  }
}