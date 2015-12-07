package calendarscript

import scala.tools.nsc.EvalLoop

import calendarscript._
import calendarscript.parser._
import calendarscript.ir._
import java.io.File
import net.fortuna.ical4j.model._
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
  def main(args: Array[String]) = {
    
    val inputFileLoc = args(0)
    val outputFolderLoc = args(1)
    
    val lines = scala.io.Source.fromFile(inputFileLoc).mkString
    CalendarParser(lines) match {
      case CalendarParser.Success(t, _) ⇒ {
        println(t)
        var (name, result) = evalRCal(t)
        scala.tools.nsc.io.File(outputFolderLoc + File.separator + name +".ics").writeAll(result.toString())
      }
      case e: CalendarParser.NoSuccess  ⇒ println(e)
    }
  }
}


