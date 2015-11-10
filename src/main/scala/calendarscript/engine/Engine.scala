package calendarscript.engine

import scala.tools.nsc.EvalLoop
import calendarscript.semantics.eval
import calendarscript.interpreter
import calendarscript.parser._
import calendarscript.ir._
import java.io.File

import net.fortuna.ical4j._
//import scalafx.application.JFXApp
/**
 * @author aputman
 */

object Engine extends interpreter {
//  override def prompt = "> "
//
//  loop { line ⇒
//    PiconotParser(line) match {
//      case PiconotParser.Success(t, _) ⇒ println(eval(t))
//      case e: PiconotParser.NoSuccess ⇒ println(e)
//    }
//  }
    val lines = scala.io.Source.fromFile("resources" + File.separator + s"mazerunner.txt").mkString
    val firstLineEnd = lines.indexOf("\n")
    println(firstLineEnd, firstLineEnd)
    val mazeName = lines.slice(0, firstLineEnd).trim
    val newLine = lines.slice(firstLineEnd, lines.length())
    PiconotParser(newLine) match {
      case PiconotParser.Success(t, _) ⇒ {
        println(t)
        eval(t)(mazeName)
      }
      case e: PiconotParser.NoSuccess  ⇒ println(e)
    }
}