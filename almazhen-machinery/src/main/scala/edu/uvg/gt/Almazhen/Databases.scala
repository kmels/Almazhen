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
      Filesystem.makeDirectory(dbname)
      val updated_dbs = databases :+ newdb
      this.setDatabasesTo(updated_dbs)
      return Some(newdb)
    }
  }
	
	/**
	 * Changes the name of the database
	 * 
	 * If successful, a Right(database) is thrown; otherwise, an execution error is returned wrapped in Left
	 */
	def alter(oldName: String, newName: String): Either[Error, Database] = {
	  val maybeOld = findByName(oldName)
	  
	  if (maybeOld.isEmpty)
	    Left(Executor.DATABASE_DOES_NOT_EXIST(oldName))
      else{
        val oldDb: Database = maybeOld.get
        
    	if (findByName(newName).nonEmpty)
    	  Left(Executor.DATABASE_ALREADY_EXISTS(newName))
    	else{
    	  val dbs = dbList.mapConserve(db => {
    	    if (db.name == oldName){
    	    	Filesystem.renameFile(oldDb.name, newName)
    	    	oldDb.copy(name = newName)
    	    }
	    	else
	    		db
    	  })
    	  
    	  setDatabasesTo(dbs)
    	  
    	  findByName(newName) match{
    	    case Some(alteredDb) => Right(alteredDb)
    	    case _ => Left(Executor.THE_IMPOSSIBLE_HAPPENED("alter table"))
    	  }
    	}
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
      Filesystem.removeDirectory(db.name)
      setDatabasesTo(dbs.filter(_ != db))
      Some(db)
    })
  }
}