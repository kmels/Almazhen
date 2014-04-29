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
  
  def mkPathFor(p: String): String = ROOT + File.separator + p
  
  def existsFile(f: String) = new java.io.File(mkPathFor(f)).exists
  
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
}