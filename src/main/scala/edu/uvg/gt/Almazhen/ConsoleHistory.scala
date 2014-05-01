package edu.uvg.gt.Almazhen

import java.io.PrintWriter
import java.io.BufferedWriter
import java.io.FileWriter
import scala.io.Source
import java.io.FileNotFoundException

object ConsoleHistory {
  final val HISTORY_FILENAME = ".console-history"
  
  def last: String = try {
    Source.fromFile(HISTORY_FILENAME).getLines().toList.last
  } catch{
      case e: FileNotFoundException => ""
  }
  
  def append(cmd: String): Unit = {
    val historyFile = new PrintWriter(new BufferedWriter(new FileWriter(HISTORY_FILENAME, true)));
    historyFile.println(cmd);
    historyFile.close();
  }
  
  
}