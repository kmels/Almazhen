package edu.uvg.gt.Almazhen

import scala.io.Source
import scala.util.Properties
import java.io
import java.io.{File, FileWriter, BufferedWriter}

/**
 * Interacts with the file system.
 */
object Filesystem {
  final val ROOT = Properties.userDir
  final val TRASH_DIRECTORY = ".Bardo"
    
  def mkPathFor(p: String): String = ROOT + File.separator + p
  
  def getFile(f: String) = new java.io.File(mkPathFor(f))
    
  def existsFile(f: String) = getFile(f).exists
  
  def readFile(filename: String): String = {
    //if it doesn't exist, create it 
    if (existsFile(filename))
      Source.fromFile(mkPathFor(filename)).mkString
	else
	  return ""
  }
  
  def writeFile(filename: String, content: String) = {
    val file = new File(filename)
	val bw = new BufferedWriter(new FileWriter(file))
	bw.write(content)
	bw.close()
  }
  
  /**
   * Creates a directory (does not directly support nested directories)
   * Returns true if the directory was created, false otherwise.
   * */
  def makeDirectory(name: String) = {
	val dir = new File(mkPathFor(name))
	dir.mkdir()
	
	existsFile(name)
  }
  
  /**
   * Returns the trash directory file.
   */
  def trashDirectoryFile: File = {
    if (!existsFile(TRASH_DIRECTORY))
      makeDirectory(TRASH_DIRECTORY)
      
    getFile(TRASH_DIRECTORY)
  }
  
  /*
   * Renames a file. 
   * Returns true if the file was moved.
   */
  def renameFile(oldfilename: String, newfilename: String) = moveFile(oldfilename, newfilename)
  
  def moveFile(oldfilename: String, newfilename: String): Boolean = {
    if ((!existsFile(oldfilename)) || (existsFile(newfilename)))
      false
    else{
      val oldFile = getFile(oldfilename)
      val newFile = new File(mkPathFor(newfilename))
      oldFile.renameTo(newFile)
      
      existsFile(newfilename)
    }
  }
  
  /**
   * Receives a filepath and moves it to the trash directory
   */
  def moveToTrash(filename: String) = {
    val file = getFile(filename)
    moveFile(filename, TRASH_DIRECTORY + File.separator + file.getName())
  }
  
  def removeDirectory(name: String) = moveToTrash(name)
  
  
}