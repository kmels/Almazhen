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
	implicit def TablesJson: CodecJson[Table] = casecodec3(Table.apply, Table.unapply) ("tablename", "columns", "restrictions")

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

  def findTableByName(tbname: String):Option[Table] = findByName(tbname) match{
    case Left(_) => None
    case Right(t) => Some(t)
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
	    Right(getfile)
	    /*getfile match{
    	    case Nil => Left(Executor.THE_IMPOSSIBLE_HAPPENED("table listing"))
    	    case _ => Right(getfile)
    	  }*/
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
      case Left(e) => Left(e) //No database selected

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
      case Left(e) => Left(e) //Table doesn't exists

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
      case Left(e) => Left(e) //Table doesn't exists

      case Right(singleTable) => {
        Right(singleTable.columns)
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
      case Left(e) => Left(e) //Table doesn't exists
      case Right(oldTB) => {
        tbList match {
          case Left(e) => Left(e) //No Database selected
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
	    	    case _ => Left(Executor.THE_IMPOSSIBLE_HAPPENED("rename table"))
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

  /**
   * Adds a new column to a table
   */
  def addColumn(tableName: String, newColumn: ColumnDefinition, newConstraints: List[Constraint]):Either[Error, Table] = {
    val maybeTable = findByName(tableName)

    maybeTable match {
      case Left(e) => Left(e) //Table doesn't exists
      case Right(currentTable) => {
        if (currentTable.columns.exists(_.name == newColumn.name))
          return Left(Executor.COLUMN_EXISTS(newColumn.name))

          val oldConstraintsNames = currentTable.restrictions.map(_.name)
          val newConstraintsNames = newConstraints.map(_.name)

          if (oldConstraintsNames.intersect(newConstraintsNames).size > 0 )
            return Left(Executor.CONSTRAINT_EXISTS(currentTable.name, oldConstraintsNames.intersect(newConstraintsNames)(0)))
          else {
            tbList match {
              case Left(e) => Left(e) //Database not selected
              case Right(tableList) =>{

                val updated_tables: List[Table] = tableList.map(t =>{
                  if (t.name == currentTable.name){
                    t.copy(columns = t.columns :+ newColumn, restrictions = t.restrictions ++ newConstraints)
                  } else
    		    	t
                })

                setTablesTo(updated_tables)
                Right(currentTable)
              } //correct procedure

            }//tbList error matching


          } //else-constraints

      }// case right
    }// safe maybeTable match
  } // addColumn definition

  /**
   * Drops an old column from a table
   */
  def dropColumn(tableName: String, columnName: String):Either[Error, Table] = {
    val maybeTable = findByName(tableName)

    maybeTable match {
      case Left(e) => Left(e) //Table doesn't exists
      case Right(currentTable) => {

        val probableColumn = currentTable.columns.find(_.name == columnName)
        if (probableColumn.isEmpty)
          return Left(Executor.COLUMN_DOES_NOT_EXISTS(columnName))

        tbList match {
          case Left(e) => Left(e) //Database not selected
          case Right(tableList) => {
            val updated_tables: List[Table] = tableList.map(t =>{
                  if (t.name == currentTable.name){
                    t.copy(columns = t.columns.filter(_.name != columnName), restrictions = t.restrictions )
                  } else
    		    	t
            })

            setTablesTo(updated_tables)
            Right(currentTable)

          }
        }

      }// case right
    }// safe maybeTable match
  } // addColumn definition

  /**
   * Adds a constraint to a table
   */
  def addConstraint(tableName: String , newConstraint: Constraint):Either[Error, Table] = {
    val maybeTable = findByName(tableName)

    maybeTable match {
      case Left(e) => Left(e) //Table doesn't exists
      case Right(currentTable) => {
        if (currentTable.restrictions.exists(_.name == newConstraint.name))
          return Left(Executor.CONSTRAINT_EXISTS(currentTable.name, newConstraint.name))

          else {
            tbList match {
              case Left(e) => Left(e) //Database not selected
              case Right(tableList) =>{

                val updated_tables: List[Table] = tableList.map(t =>{
                  if (t.name == currentTable.name){
                    t.copy(columns = t.columns , restrictions = t.restrictions :+ newConstraint)
                  } else
              t
                })

                setTablesTo(updated_tables)
                Right(currentTable)
              } //correct procedure

            }//tbList error matching


          } //else-constraints

      }// case right
    }// safe maybeTable match
  } // addColumn definition

  /**
   * Drops a constraint from a table
   */
  def dropConstraint(tableName: String, oldConstraint: String):Either[Error, Table] = {
    val maybeTable = findByName(tableName)

    maybeTable match {
      case Left(e) => Left(e) //Table doesn't exists
      case Right(currentTable) => {

        val probableColumn = currentTable.restrictions.find(_.name == oldConstraint)
        if (probableColumn.isEmpty)
          return Left(Executor.CONSTRAINT_DOES_NOT_EXISTS(tableName, oldConstraint))

        tbList match {
          case Left(e) => Left(e) //Database not selected
          case Right(tableList) => {
            val updated_tables: List[Table] = tableList.map(t =>{
                  if (t.name == currentTable.name){
                    t.copy(columns = t.columns, restrictions = t.restrictions.filter(_.name != oldConstraint) )
                  } else
              t
            })

            setTablesTo(updated_tables)
            Right(currentTable)

          }
        }

      }// case right
    }// safe maybeTable match
  } // addColumn definition



}//tables Object
