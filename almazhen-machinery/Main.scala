/**
 * Created by kmels on 3/20/14.
 */

import java.io.{InputStreamReader, BufferedReader}
import java.util.Scanner;
import edu.uvg.gt.Almazhen._

object Main extends App{
  val scanner = new Scanner(System.in)

  override def main(args: Array[String]) = loop

  def loop : Unit = {
    Console.print("Your command: ")
    Console.flush();

    val buffer = new BufferedReader(new InputStreamReader(System.in));
    var input = buffer.readLine();

    if ((input != null) && (input.equals(":it"))){
      print("Your command: "+ConsoleHistory.last + "\n")
      Executor.parseAndExec(ConsoleHistory.last, loop)
    }else{
      Executor.parseAndExec(input, loop)
    }
  }

  
}
