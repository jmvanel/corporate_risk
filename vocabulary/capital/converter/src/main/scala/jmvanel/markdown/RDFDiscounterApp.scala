/*
## Converting Markdown to Turtle ##

The `Discounter` is what grabs the "knockoff" of a markdown string. For simple usage,
you can use the `DefaultDiscounter` object.

    import jmvanel.markdown.RDFDiscounterApp._
    import com.tristanhunt.knockoff._
    toTTL( knockoff( markdownString ) )
*/
package jmvanel.markdown

import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Position
import scala.util.parsing.input.CharSequenceReader
import com.tristanhunt.knockoff._

/*

### The RDF Discounter

Provides an object along with a main method for the "good enough to script" kind of
usage. Note that a major aim of this discounter is to mimic the usage of
`Markdown.pl`.

    Markdown.pl [ −−html4tags ] [ −−version ] [ −shortversion ] [ file ... ]

The `--html4tags` argument will just do nothing, but not be processed as a file.

*/

import java.io.File

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
      args.filter(_ != "--html4tags").foreach {
        fileName =>
          println(toTTL(knockoff(readText(fileName))).toString)
      }
    }
  } catch {
    case th: Throwable => {
      th.printStackTrace(Console.err)
    }
  }

  private def readText(fileName: String): String =
    io.Source.fromFile(new File(fileName)).mkString("")
}
