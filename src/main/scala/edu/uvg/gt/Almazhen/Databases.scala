package edu.uvg.gt.Almazhen

import scalaz._, Scalaz._
import argonaut._, Argonaut._

object Databases {
	final val DB_METAFILE = "databases.meta"
	implicit def DatabasesJson: CodecJson[Database] = casecodec2(Database.apply, Database.unapply)("name", "tables_count")
	private var currentDatabase: Option[Database] = None
	
	private def findByName(dbname: String): Option[Database] = dbList.find(_.name == dbname)
	
	/**
	 * Changes the current database in use
	 * 
	 * If the database name exists, the database object is returned. Otherwise, we return None
	 */
	def use(dbname: String): Option[Database] = this.findByName(dbname) match {
	  case Some(db) => {
	    currentDatabase = Some(db)
	    current
	  }
	  case _ => None
	}
	
	/**
	 * Returns the current database
	 */
	def current : Option[Database] = currentDatabase
	
  /**
   * Creates a new database.
   * 
   * If the database exists, it's left intact and None is returned.
   */
  def create(dbname: String): Option[Database] = {
    val databases = dbList
    
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
  
  /**
   * Drops an existent database.
   * 
   * If the database does not exist, None is returned.
   * Otherwise, the just-dropped database is returned 
   */
  def drop(dbname: String): Option[Database] = {
    val dbs = dbList
    val maybeDB = dbs.find(_.name == dbname)
    
    maybeDB.fold[Option[Database]](None)(db => {
      //delete and return it
      setDatabasesTo(dbs.filter(_ != db))
      Some(db)
    })
  }
}