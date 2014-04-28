/**
 * Created by kmels on 3/20/14.
 */

import java.io.{InputStreamReader, BufferedReader}
import java.util.Scanner;
import edu.uvg.gt.Almazhen._

object Main  extends App{
  val scanner = new Scanner(System.in)

  override def main(args: Array[String]) = loop

  def loop : Unit = {
    Console.print("Your command: ")
    Console.flush();

    val buffer = new BufferedReader(new InputStreamReader(System.in));
    val input = buffer.readLine();

    val parseResult : Option[Command] = Parser.parse(input)
    val cmdParseResult = parseResult.toString()
    println(s"Acknowledged this: $cmdParseResult")

    if (input != "exit")
      loop
  }

}
