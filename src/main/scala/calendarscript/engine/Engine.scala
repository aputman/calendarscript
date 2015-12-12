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
  def main(args: Array[String]) = {
    val firstArg = args(0)
    val secondArg = args(1)
    var lines = "";
    if (firstArg == "-raw") {
      lines = secondArg;
    }
    else {
      lines = scala.io.Source.fromFile(firstArg).mkString
    }
    CalendarParser(lines) match {
      case CalendarParser.Success(t, _) ⇒ {
        
        var (name, result) = evalRCal(t)
        if (firstArg == "-raw") {
          println(result.toString())
        }
        else {
          scala.tools.nsc.io.File(secondArg + File.separator + name +".ics").writeAll(result.toString())
        }
      }
      case e: CalendarParser.NoSuccess  ⇒ println(e)
    }
  }
}


