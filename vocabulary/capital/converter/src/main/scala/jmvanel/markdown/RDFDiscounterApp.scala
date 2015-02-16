/*
## Converting Markdown to Turtle ##

The `Discounter` is what grabs the "knockoff" of a markdown string.

    import jmvanel.markdown.RDFDiscounterApp._
    import com.tristanhunt.knockoff._
    toTTL( knockoff( markdownString ) )
*/
package jmvanel.markdown

import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Position
import scala.util.parsing.input.CharSequenceReader
import com.tristanhunt.knockoff._
import java.io.File
import java.nio.file.StandardOpenOption

object RDFDiscounterApp extends Discounter with RDFWriter {
  def main(args: Array[String]): Unit = try {
    if (args.contains("--version")) {
      Console.err.print("DefaultDiscounter ")
    }
    if (args.contains("--version") || args.contains("-shortversion")) {
      Console.err.println("0.7.1-SNAPSHOT")
      return
    }

    if (args.isEmpty) {
      val sb = new StringBuilder
      var line: String = null
      do {
        line = Console.readLine
        if (line != null) sb.append(line)
      } while (line != null)
      println(toTTL(knockoff(sb.toString)).toString)
    } else {
//      args.filter(_ != "--html4tags").foreach {
//        fileName =>
      val fileName = args(0)
      val initialIndex = if( args.size > 1 ) args(1).toInt else 0
      index = initialIndex
    	printTurtleFile(toTTL(knockoff(readText(fileName))), fileName )
//      }
    }
  } catch {
    case th: Throwable => {
      th.printStackTrace(Console.err)
    }
  }

  private def printTurtleFile(ttl: String, fileName: String) = {
    val ttlFileName = fileName.replaceFirst("""\.md$""", ".ttl")
    import java.nio.file.{ Paths, Files }
    import java.nio.charset.StandardCharsets
    Files.write(Paths.get(ttlFileName), ttl.getBytes(StandardCharsets.UTF_8)
//        , StandardOpenOption.APPEND,
//        StandardOpenOption.CREATE
        )
    println(s"Writen ${ttl.length()} characters to file \n $ttlFileName")
  }

  private def readText(fileName: String): String =
    io.Source.fromFile(new File(fileName)).mkString("")

  private def readToList(fileName: String): List[String] =
    io.Source.fromFile(new File(fileName)).getLines().toList
}
